
structure Interpret :> INTERPRET =
   struct

      structure B = Bytestring
      structure BS = Bytesubstring
      structure S = Script

      type value = Bytestring.string
      type stack = value list

      exception Reject

      val zero = B.str 0w0
      val one = B.str 0w1
      val oneTwentyEight = B.str 0wx80


      fun intToValue x =
         if x = 0 then
            B.null
         else if x > 0 then
            let
               val str = ConvertIntInf.toBytesL x
               val sz = B.size str
            in
               if Word8.andb (B.sub (str, sz-1), 0wx80) = 0w0 then
                  str
               else
                  B.^ (str, zero)
            end
         else
            let
               val str = ConvertIntInf.toBytesL (~x)
               val sz = B.size str
               val hi = B.sub (str, sz-1)
            in
               if Word8.andb (hi, 0wx80) = 0w0 then
                  B.^ (B.substring (str, 0, sz-1), B.str (Word8.orb (hi, 0wx80)))
               else
                  B.^ (str, oneTwentyEight)
            end

      fun valueToInt str =
         let
            val sz = B.size str
         in
            if sz = 0 then
               0
            else
               let
                  val hi = B.sub (str, sz-1)
               in
                  if Word8.andb (hi, 0wx80) = 0w0 then
                     ConvertIntInf.fromBytesL str
                  else
                     ~ (ConvertIntInf.fromBytesL (B.^ (B.substring (str, 0, sz-1), B.str (Word8.andb (hi, 0wx7f)))))
               end
         end

      fun valueToBool str =
         let
            val sz = B.size str

            fun loop i =
               if i = sz-1 then
                  B.sub (str, i) <> 0w0 andalso B.sub (str, i) <> 0wx80  (* negative zero is still false *)
               else
                  B.sub (str, i) <> 0w0 orelse loop (i+1)
         in
            if sz = 0 then
               false
            else
               loop 0
         end

      fun boolToValue b =
         if b then
            one
         else
            B.null



      fun dest stack =
         (case stack of
             [] =>
                raise Reject
           | v :: rest =>
                (v, rest))

      fun dest2 stack =
         (case stack of
             v1 :: v2 :: rest =>
                (v1, v2, rest)
           | _ =>
                raise Reject)

      fun dest3 stack =
         (case stack of
             v1 :: v2 :: v3 :: rest =>
                (v1, v2, v3, rest)
           | _ =>
                raise Reject)

      fun destnRev n stack =
         let
            fun loop n stack acc =
               if n = 0 then
                  (acc, stack)
               else
                  (case stack of
                      [] =>
                         raise Reject
                    | v :: rest =>
                         loop (n-1) rest (v :: acc))
         in
            if n < 0 then
               (* This will happen anyway, but let's be more direct about it. *)
               raise Reject
            else
               loop n stack []
         end

      fun destn n stack =
         let
            val (prefixRev, stack') = destnRev n stack
         in
            (rev prefixRev, stack')
         end

      fun destMultisig stack =
         let
            val (vNumPubkeys, stack') = dest stack
            val numPubkeys = IntInf.toInt (valueToInt vNumPubkeys)
            val (pubkeys, stack') = destn numPubkeys stack'

            val () =
               if numPubkeys > 20 then
                  raise Reject
               else
                  ()

            val (vNumSigs, stack') = dest stack'
            val numSigs = IntInf.toInt (valueToInt vNumSigs)

            val () =
               if numSigs > numPubkeys then
                  raise Reject
               else
                  ()

            val (sigs, stack') = destn numSigs stack'

            (* gratuitously pop one more element from the stack *)
            val (_, stack') = dest stack'
         in
            (numSigs, sigs, numPubkeys, pubkeys, stack')
         end



      fun decode code =
         Reader.readS Script.instReader code
         handle Reader.SyntaxError => raise Reject


      (* A transaction environment contains the information needed to validate signatures:
         1. The transaction on behalf of which the script is being run.
         2. The index of the transaction's input on behalf of which the script is being run.
         3. The code of the script (more precisely, the suffix that follows the most recent Codeseparator).
      *)
      type txenv = Transaction.tx * int * BS.substring


      (* Opcodes over 0x60 are unusual, except DUP, HASH160, EQUALVERIFY, CHECKSIG, CHECKMULTSIG, NOP, EQUAL.
         PUSHDATA4 and RESERVED (0x50) are also unusual.
         (Opcodes 0x60 and below -- other than 0x50 -- push constants.)
      *)
         
      val unusual = Array.tabulate (256, (fn i => i > 0x60))
      val () =
         List.app (fn i => Array.update (unusual, i, true))
         [0x4e, 0x50]
      val () =
         List.app (fn i => Array.update (unusual, i, false))
         [0x76, 0xa9, 0x88, 0xac, 0xae, 0x61, 0x87]


      (* stksz = |stack| + |altstack|, which may not exceed Constants.maxStackSize
         ops is the current number of (non-constant) operations, which may not exceed Constants.maxScriptOperations
         txenv is the current transaction environment

      *)
      fun run code stack altstack stksz ops ifstack (txenv : txenv) =
         let
            fun run' code stack altstack stksz ops =
               if stksz > Constants.maxStackSize orelse ops > Constants.maxScriptOperations then
                  raise Reject
               else if BS.isEmpty code then
                  (case ifstack of
                      [] =>
                         stack
                    | _ =>
                         (* Fail if the script ends with a nonempty if stack. *)
                         raise Reject)
               else
                  let
                     val (inst, cont) = decode code

                     val () =
                        if Array.sub (unusual, Word8.toInt (BS.sub (code, 0))) then
                           Log.long (fn () => "Unusual: "^ Script.instToString inst)
                        else
                           ()
                  in
                     (case inst of
                         S.Const str =>
                            if B.size str > Constants.maxConstantSize then
                               raise Reject
                            else
                               run' cont (str :: stack) altstack (stksz+1) ops
     
                       | S.Constn x =>
                            (* invariant: 1 <= x <= 16, so the size won't exceed 520 *)
                            run' cont (intToValue x :: stack) altstack (stksz+1) ops
     
                       | S.MinusOne =>
                            run' cont (intToValue (~1) :: stack) altstack (stksz+1) ops
     
                       | S.Nop =>
                            run' cont stack altstack stksz (ops+1)

                       | S.If =>
                            let
                               val (v, stack') = dest stack
                            in
                               run cont stack' altstack (stksz-1) (ops+1) (valueToBool v :: ifstack) txenv
                            end
     
                       | S.Notif =>
                            let
                               val (v, stack') = dest stack
                            in
                               run cont stack' altstack (stksz-1) (ops+1) (not (valueToBool v) :: ifstack) txenv
                            end

                       | S.Else =>
                            (* The semantics of Else are a bit strange.  Multiple elses are permitted,
                               and every other segment (delimited by an Else) is executed.
                            *)
                            let
                               val (b, ifstack') = dest ifstack
                            in
                               run cont stack altstack stksz (ops+1) (not b :: ifstack') txenv
                            end
     
                       | S.Endif =>
                            let
                               val (_, ifstack') = dest ifstack
                            in
                               run cont stack altstack stksz (ops+1) ifstack' txenv
                            end

                       | S.Verify =>
                            let
                               val (v, stack') = dest stack
                            in
                               if valueToBool v then
                                  run' cont stack' altstack (stksz-1) (ops+1)
                               else
                                  raise Reject
                            end
     
                       | S.Return =>
                            raise Reject
     
                       | S.ToAltstack =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont stack' (v :: altstack) stksz (ops+1)
                            end
     
                       | S.FromAltstack =>
                            let
                               val (v, altstack') = dest altstack
                            in
                               run' cont (v :: stack) altstack' stksz (ops+1)
                            end
     
                       | S.Ifdup =>
                            let
                               val (v, _) = dest stack
                            in
                               if valueToBool v then
                                  run' cont (v :: stack) altstack (stksz+1) (ops+1)
                               else
                                  run' cont stack altstack stksz (ops+1)
                            end
     
                       | S.Depth =>
                            run' cont (intToValue (IntInf.fromInt (length stack)) :: stack) altstack (stksz+1) (ops+1)
     
                       | S.Drop =>
                            let
                               val (_, stack') = dest stack
                            in
                               run' cont stack' altstack (stksz-1) (ops+1)
                            end
     
                       | S.Dup =>
                            let
                               val (v, _) = dest stack
                            in
                               run' cont (v :: stack) altstack (stksz+1) (ops+1)
                            end
     
                       | S.Nip =>
                            let
                               val (v1, _, stack') = dest2 stack
                            in
                               run' cont (v1 :: stack') altstack (stksz-1) (ops+1)
                            end
     
                       | S.Over =>
                            let
                               val (_, v2, _) = dest2 stack
                            in
                               run' cont (v2 :: stack) altstack (stksz+1) (ops+1)
                            end
     
                       | S.Pick =>
                            let
                               val (nv, stack') = dest stack
                               val n = IntInf.toInt (valueToInt nv)
     
                               val v =
                                  List.nth (stack', n)
                                  handle Subscript => raise Reject
                            in
                               run' cont (v :: stack') altstack (stksz+1) (ops+1)
                            end
     
                       | S.Roll =>
                            let
                               val (nv, stack') = dest stack
                               val (prefixRev, stack') = destnRev (IntInf.toInt (valueToInt nv)) stack'
                               val (v, stack') = dest stack'
                            in
                               run' cont (v :: List.revAppend (prefixRev, stack')) altstack stksz (ops+1)
                            end
     
                       | S.Rot =>
                            let
                               val (v1, v2, v3, stack') = dest3 stack
                            in
                               run' cont (v3 :: v1 :: v2 :: stack') altstack stksz (ops+1)
                            end
     
                       | S.Swap =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (v2 :: v1 :: stack') altstack stksz (ops+1)
                            end
     
                       | S.Tuck =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (v1 :: v2 :: v1 :: stack') altstack (stksz+1) (ops+1)
                            end
     
                       | S.TwoDrop =>
                            let
                               val (_, _, stack') = dest2 stack
                            in
                               run' cont stack' altstack (stksz-2) (ops+1)
                            end
     
                       | S.TwoDup =>
                            let
                               val (v1, v2, _) = dest2 stack
                            in
                               run' cont (v1 :: v2 :: stack) altstack (stksz+2) (ops+1)
                            end
     
                       | S.ThreeDup =>
                            let
                               val (v1, v2, v3, _) = dest3 stack
                            in
                               run' cont (v1 :: v2 :: v3 :: stack) altstack (stksz+3) (ops+1)
                            end
     
                       | S.TwoOver =>
                            (case stack of
                                v1 :: v2 :: v3 :: v4 :: _ =>
                                   run' cont (v3 :: v4 :: stack) altstack (stksz+2) (ops+1)
                              | _ =>
                                   raise Reject)
     
                       | S.TwoRot =>
                            (case stack of
                                v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: stack' =>
                                   run' cont (v5 :: v6 :: v1 :: v2 :: v3 :: v4 :: stack') altstack stksz (ops+1)
                              | _ =>
                                   raise Reject)
     
                       | S.TwoSwap =>
                            (case stack of
                                v1 :: v2 :: v3 :: v4 :: stack' =>
                                   run' cont (v3 :: v4 :: v1 :: v2 :: stack') altstack stksz (ops+1)
                              | _ =>
                                   raise Reject)

                       | S.Cat =>
                            (* disabled *)
                            raise Reject
     
                       | S.Substr =>
                            (* disabled *)
                            raise Reject
     
                       | S.Left =>
                            (* disabled *)
                            raise Reject
     
                       | S.Right =>
                            (* disabled *)
                            raise Reject

                       | S.Size =>
                            let
                               val (v, _) = dest stack
                            in
                               run' cont (intToValue (IntInf.fromInt (B.size v)) :: stack) altstack (stksz+1) (ops+1)
                            end
     
                       | S.Invert =>
                            (* disabled *)
                            raise Reject
     
                       | S.And =>
                            (* disabled *)
                            raise Reject
     
                       | S.Or =>
                            (* disabled *)
                            raise Reject
     
                       | S.Xor =>
                            (* disabled *)
                            raise Reject

                       | S.Equal =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (B.eq (v1, v2)) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.EqualVerify =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               if B.eq (v1, v2) then
                                  run' cont stack' altstack (stksz-2) (ops+1)
                               else
                                  raise Reject
                            end

                       | S.OneAdd =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (intToValue (valueToInt v + 1) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.OneSub =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (intToValue (valueToInt v - 1) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.TwoMul =>
                            (* disabled *)
                            raise Reject

                       | S.TwoDiv =>
                            (* disabled *)
                             raise Reject

                       | S.Negate =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (intToValue (~ (valueToInt v)) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.Abs =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (intToValue (IntInf.abs (valueToInt v)) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.Not =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (boolToValue (not (valueToBool v)) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.ZeroNotEqual =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (boolToValue (valueToBool v) :: stack') altstack stksz (ops+1)
                            end
     
                       | S.Add =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (intToValue (valueToInt v1 + valueToInt v2) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Sub =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (intToValue (valueToInt v2 - valueToInt v1) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Mul =>
                            (* disabled *)
                            raise Reject

                       | S.Div =>
                            (* disabled *)
                            raise Reject

                       | S.Mod =>
                            (* disabled *)
                            raise Reject

                       | S.Lshift =>
                            (* disabled *)
                            raise Reject

                       | S.Rshift =>
                            (* disabled *)
                            raise Reject

                       | S.BoolAnd =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToBool v1 andalso valueToBool v2) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.BoolOr =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToBool v1 orelse valueToBool v2) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.NumEqual =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v1 = valueToInt v2) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.NumEqualVerify =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               if valueToInt v1 = valueToInt v2 then
                                  run' cont stack' altstack (stksz-2) (ops+1)
                               else
                                  raise Reject
                            end

                       | S.NumNotEqual =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v1 <> valueToInt v2) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Lt =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v2 < valueToInt v1) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Gt =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v2 > valueToInt v1) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Leq =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v2 <= valueToInt v1) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Geq =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (boolToValue (valueToInt v2 >= valueToInt v1) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Min =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (intToValue (IntInf.min (valueToInt v1, valueToInt v2)) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Max =>
                            let
                               val (v1, v2, stack') = dest2 stack
                            in
                               run' cont (intToValue (IntInf.max (valueToInt v1, valueToInt v2)) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.Within =>
                            let
                               val (v1, v2, v3, stack') = dest3 stack
                               val x = valueToInt v3
                               val min = valueToInt v2
                               val max = valueToInt v1
                            in
                               run' cont (boolToValue (IntInf.<= (min, x) andalso IntInf.< (x, max)) :: stack') altstack (stksz-2) (ops+1)
                            end

                       | S.Ripemd160 =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (RIPEMD160.hashBytes v :: stack') altstack stksz (ops+1)
                            end

                       | S.Sha256 =>
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (SHA256.hashBytes v :: stack') altstack stksz (ops+1)
                            end

                       | S.Hash160 => 
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (RIPEMD160.hashBytes (SHA256.hashBytes v) :: stack') altstack stksz (ops+1)
                            end

                       | S.Sha1 =>
                            raise (Fail "SHA1 unimplemented")

                       | S.Hash256 => 
                            let
                               val (v, stack') = dest stack
                            in
                               run' cont (SHA256.hashBytes (SHA256.hashBytes v) :: stack') altstack stksz (ops+1)
                            end

                       | S.Codeseparator =>
                            let
                               val (tx, i, _) = txenv
                            in
                               run cont stack altstack stksz (ops+1) ifstack (tx, i, cont)
                            end
                            
                       | S.Checksig =>
                            let
                               val (pubkey, sg, stack') = dest2 stack
                               val (tx, i, script) = txenv
                            in
                               run' cont (boolToValue (Signature.verify tx i script [sg] [pubkey]) :: stack') altstack (stksz-1) (ops+1)
                            end

                       | S.ChecksigVerify =>
                            let
                               val (pubkey, sg, stack') = dest2 stack
                               val (tx, i, script) = txenv
                            in
                               if Signature.verify tx i script [sg] [pubkey] then
                                  run' cont stack' altstack (stksz-1) (ops+1)
                               else
                                  raise Reject
                            end

                       | S.Checkmultisig =>
                            let
                               val (numSigs, sigs, numPubkeys, pubkeys, stack') = destMultisig stack

                               val ops' = ops+1+numPubkeys
                               val () =
                                  if ops' > Constants.maxScriptOperations then
                                     raise Reject
                                  else
                                     ()

                               val (tx, i, script) = txenv
                            in
                               run' cont (boolToValue (Signature.verify tx i script sigs pubkeys) :: stack') altstack (stksz-numSigs-numPubkeys-2) ops'
                            end

                       | S.CheckmultisigVerify =>
                            let
                               val (numSigs, sigs, numPubkeys, pubkeys, stack') = destMultisig stack

                               val ops' = ops+1+numPubkeys
                               val () =
                                  if ops' > Constants.maxScriptOperations then
                                     raise Reject
                                  else
                                     ()

                               val (tx, i, script) = txenv
                            in
                               if Signature.verify tx i script sigs pubkeys then
                                  run' cont stack' altstack (stksz-numSigs-numPubkeys-3) ops'
                               else
                                  raise Reject
                            end

                      | S.Reserved =>
                           raise Reject

                      | S.Invalid =>
                           raise Reject

                       | S.Verif =>
                            raise Reject

                       | S.Vernotif =>
                            raise Reject)
                  end

            fun skip code ops =
               if ops > Constants.maxScriptOperations then
                  raise Reject
               else if BS.isEmpty code then
                  (* Fail if the script ends with a nonempty if stack, which we must have
                     if false is on the stack.
                  *)
                  raise Reject
               else
                  let
                     val (inst, cont) = decode code

                     val () =
                        if Array.sub (unusual, Word8.toInt (BS.sub (code, 0))) then
                           Log.long (fn () => "Unusual: "^ Script.instToString inst)
                        else
                           ()
                  in
                     if ops > Constants.maxScriptOperations then
                        raise Reject
                     else
                        (case inst of
                            S.If =>
                               (* It doesn't matter what we put on the stack. *)
                               run cont stack altstack stksz (ops+1) (false :: ifstack) txenv
                          | S.Notif =>
                               (* It doesn't matter what we put on the stack. *)
                               run cont stack altstack stksz (ops+1) (false :: ifstack) txenv
                          | S.Else =>
                               (* The semantics of Else are a bit strange.  Multiple elses are permitted,
                                  and every other segment (delimited by an Else) is executed.
                               *)
                               (case ifstack of
                                   [] =>
                                      raise (Fail "impossible")
                                 | b :: ifstack' =>
                                      run cont stack altstack stksz (ops+1) (not b :: ifstack') txenv)
                          | S.Endif =>
                               (case ifstack of
                                   [] =>
                                      raise (Fail "impossible")
                                 | _ :: ifstack' =>
                                      run cont stack altstack stksz (ops+1) ifstack' txenv)
                          | S.Verif =>
                               raise Reject
                          | S.Vernotif =>
                               raise Reject
                          | _ =>
                               skip cont (ops+1))
                  end
         in
            (* It shouldn't be necessary to check stksz here, since no operation that increases the stack size
               calls run, but it seems more robust just to do it.
            *)
            if stksz > Constants.maxStackSize orelse ops > Constants.maxScriptOperations  then
               raise Reject
            else if List.all (fn b => b) ifstack then
               run' code stack altstack stksz ops
            else
               (* In the unselected arm of If or Notif.  Ignore everything but branching instructions. *)
               skip code ops
         end


      fun exec tx i code stack =
         let
            val code' = BS.full code
         in
            run code' stack [] (length stack) 0 [] (tx, i, code')
         end


      fun passes stack =
         (case stack of
             [] =>
                false
           | v :: _ =>
                valueToBool v)

   end
