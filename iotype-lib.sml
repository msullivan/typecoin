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
	end
end

(*  structure IOInt16 = IOInt (Int16) *) (* not in SML/NJ arghlbalrgh *)
structure IOInt31 = IOIntFun (Int31)
structure IOInt32 = IOIntFun (Int32)
structure IOInt = IOIntFun (Int)

structure IOString = struct
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

structure IOTypes =
struct


  val readInt = IOInt.readInt
  val writeInt = IOInt.writeInt
  val readString = IOString.readString
  val writeString = IOString.writeString
  val writeList = IOList.writeList
  val readList = IOList.readList


end
