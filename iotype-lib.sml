(* This if from the IOType support lib, hacked up by Michael Sullivan
 * to have just what I need, in the places I want, and working in SML/NJ. *)

(*
	IOType
	Copyright 2011 Christopher Cramer

	Permission to use, copy, modify, and distribute this software and its
	documentation for any purpose and without fee is hereby granted,
	provided that the above copyright notice appear in all copies and that
	both the copyright notice and this permission notice and warranty
	disclaimer appear in supporting documentation, and that the name of
	the above copyright holders, or their entities, not be used in
	advertising or publicity pertaining to distribution of the software
	without specific, written prior permission.

	The above copyright holders disclaim all warranties with regard to
	this software, including all implied warranties of merchantability and
	fitness. In no event shall the above copyright holders be liable for
	any special, indirect or consequential damages or any damages
	whatsoever resulting from loss of use, data or profits, whether in an
	action of contract, negligence or other tortious action, arising out
	of or in connection with the use or performance of this software.
*)


functor IOIntFun (Integer: INTEGER) = struct
    type elem = Integer.int

    local
        fun fixed n =
            let
                val (bytesPerElem, subVecX, update) =
                    if Int.<= (n, 8) then (
                        1
                        , Word8.toLargeX o Word8Vector.sub
                        , fn (a, i, w) =>
                            Word8Array.update (
                                a
                                , i
                                , Word8.fromLarge w
                            )
                    ) else if Int.<= (n, 16) then (
                        2
                        , PackWord16Little.subVecX
                        , PackWord16Little.update
                    ) else if Int.<= (n, 32) then (
                        4
                        , PackWord32Little.subVecX
                        , PackWord32Little.update
                    ) else raise Fail "unsupported int size"
            in {
                readInt = fn p =>
                    let
                        val v = BinIO.inputN (p, bytesPerElem)
                    in
                        if Int.< (Word8Vector.length v, bytesPerElem) then
                            NONE
                        else SOME (
                            Integer.fromLarge (
                                LargeWord.toLargeIntX (
                                    subVecX (v, 0)
                                )
                            )
                        )
                    end
                , writeInt = fn (p, x) =>
                    let
                        val a = Word8Array.array (bytesPerElem, 0w0)
                    in
                        update (a, 0, LargeWord.fromLargeInt (Integer.toLarge x))
                        ; BinIO.output (p, Word8Array.vector a)
                    end
            } end
    in
        val {readInt, writeInt} = case Integer.precision of
            NONE => raise Fail "unsupported"
            | SOME n => fixed n
                val (readElem, writeElem) = (readInt, writeInt)
    end
end

(*  structure IOInt16 = IOInt (Int16) *) (* not in SML/NJ arghlbalrgh *)
structure IOInt31 = IOIntFun (Int31)
structure IOInt32 = IOIntFun (Int32)
structure IOInt = IOIntFun (Int)

functor IOWordFun (Word: WORD) = struct
        type elem = Word.word

    local
        val (bytesPerElem, subVec, update) =
            if Int.<= (Word.wordSize, 8) then (
                1
                , Word8.toLarge o Word8Vector.sub
                , fn (a, i, w) =>
                    Word8Array.update (a, i, Word8.fromLarge w)
            ) else if Int.<= (Word.wordSize, 16) then (
                2
                , PackWord16Little.subVec
                , PackWord16Little.update
            ) else if Int.<= (Word.wordSize, 32) then (
                4
                , PackWord32Little.subVec
                , PackWord32Little.update
            ) else raise Fail "unsupported word size"
    in
        fun readWord p =
            let
                val v = BinIO.inputN (p, bytesPerElem)
            in
                if Int.< (Word8Vector.length v, bytesPerElem) then NONE
                else SOME (Word.fromLarge (subVec (v, 0)))
            end
        fun writeWord (p, w) =
            let
                val a = Word8Array.array (bytesPerElem, 0w0)
            in
                update (a, 0, Word.toLarge w)
                ; BinIO.output (p, Word8Array.vector a)
            end
                val (readElem, writeElem) = (readWord, writeWord)
    end
end

structure IOWord8 = IOWordFun (Word8)
structure IOWord32 = IOWordFun (Word32)
structure IOWord = IOWordFun (Word)

structure IOString = struct
    type elem = string
    fun writeString (p, x) = (
        IOInt.writeInt (p, size x)
        ; BinIO.output (p, Byte.stringToBytes x)
    )
    fun readString p = case IOInt.readInt p of
        NONE => NONE
        | SOME n =>
            let
                val v = BinIO.inputN (p, n)
            in
                if Int.< (Word8Vector.length v, n) then NONE
                else SOME (Byte.bytesToString v)
            end
        val (readElem, writeElem) = (readString, writeString)
end

structure IOBool = struct
    type elem = bool
    fun writeBool (p, x) = BinIO.output1 (p, if x then 0w1 else 0w0)
    fun readBool p = case BinIO.input1 p of
        SOME 0w0 => SOME false
        | SOME 0w1 => SOME true
        | _ => NONE
end


structure IOList = struct
    local
        fun countAndReverse l =
            let
                fun loop (r, n, m) = case m of
                    nil => (r, n)
                    | x :: y => loop (x :: r, n + 1, y)
            in
                loop (nil, 0, l)
            end
    in
        fun writeList w (p, l) =
            let
                val (r, n) = countAndReverse l
            in
                IOInt.writeInt (p, n)
                ; app (fn x => w (p, x)) r
            end
    end
    fun readList r p = case IOInt.readInt p of
        NONE => NONE
        | SOME n =>
            let
                fun loop (l, i) =
                    if i = n then SOME l
                    else case r p of
                        NONE => NONE
                        | SOME x => loop (x :: l, i + 1)
            in
                loop (nil, 0)
            end
end

signature ELEMENT = sig
    type elem
    val writeElem: BinIO.outstream * elem -> unit
    val readElem: BinIO.instream -> elem option
end

functor IOMonoVector (
    structure Element: ELEMENT
    structure Vector: MONO_VECTOR where type elem = Element.elem
) = struct
    open Vector
    val writeElem = Element.writeElem
    val readElem = Element.readElem
    fun writeVector (p, v) = (
        IOInt.writeInt (p, length v)
        ; app (fn x => Element.writeElem (p, x)) v
    )
    fun readVector p = case IOInt.readInt p of
        NONE => NONE
        | SOME n =>
            let
                exception Eof
            in
                SOME (tabulate (n, fn _ => case Element.readElem p of
                    NONE => raise Eof
                    | SOME x => x
                )) handle Eof => NONE
            end
        val (readElem, writeElem) = (readVector, writeVector)
end

structure IOCharVector = struct
    val readVector = IOString.readString
    val writeVector = IOString.writeString
        val (readElem, writeElem) = (readVector, writeVector)
end
structure IOWord8Vector = IOMonoVector (
    structure Element = IOWord8
    structure Vector = Word8Vector
)


structure IOTypes =
struct


  val readInt = IOInt.readInt
  val writeInt = IOInt.writeInt
  val readBool = IOBool.readBool
  val writeBool = IOBool.writeBool
  val readString = IOString.readString
  val writeString = IOString.writeString
  val writeList = IOList.writeList
  val readList = IOList.readList
  val readWord8Vector = IOWord8Vector.readVector
  val writeWord8Vector = IOWord8Vector.writeVector

  fun writeToVector writer object =
      let val (data, stream) = VectorWriterBin.mkVectorWriter ();
          val () = writer (stream, object)
      in VectorWriterBin.getWriterData data end

  fun readFromVector reader vector =
      reader (VectorWriterBin.openVector vector)

end
