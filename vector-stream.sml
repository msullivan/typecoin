(* this should probably go in cmlib *)

structure VectorWriterBin =
struct

  structure V = Word8Vector
  structure VS = Word8VectorSlice
  structure IOS = BinIO
  structure Prim = BinPrimIO
  type vector_writer_data = VS.slice list ref

  fun mkVectorWriter () =
      let val data: vector_writer_data = ref []
          val stream = Prim.augmentWriter (Prim.WR {
                       name = "null",
                       chunkSize = 512,
                       writeVec = SOME (fn vec => (data := vec :: !data;
                                                   VS.length vec)),
                       writeArr = NONE,
                       writeVecNB = NONE,
                       writeArrNB = NONE,
                       block = NONE,
                       canOutput = SOME (fn () => true),
                       getPos = NONE,
                       setPos = NONE,
                       endPos = NONE,
                       verifyPos = NONE,
                       (* We should perhaps actually close. *)
                       close = (fn () => ()),
                       ioDesc = NONE
                       })
          val stream' = IOS.mkOutstream (
                        IOS.StreamIO.mkOutstream (stream, IO.NO_BUF))
      in (data, stream') end

  fun getWriterData data =
      let val vec = VS.concat (rev (!data))
          (* Update the list with the new concatenated thing *)
          val () = data := [VS.full vec]
      in vec end

  val empty_vec = V.fromList []
  fun openVector v = IOS.mkInstream (
                     IOS.StreamIO.mkInstream (Prim.openVector v, empty_vec))

end


structure VectorWriterText =
struct

  structure V = CharVector
  structure VS = CharVectorSlice
  structure IOS = TextIO
  structure Prim = TextPrimIO
  type vector_writer_data = VS.slice list ref

  fun mkVectorWriter () =
      let val data: vector_writer_data = ref []
          val stream = Prim.augmentWriter (Prim.WR {
                       name = "null",
                       chunkSize = 512,
                       writeVec = SOME (fn vec => (data := vec :: !data;
                                                   VS.length vec)),
                       writeArr = NONE,
                       writeVecNB = NONE,
                       writeArrNB = NONE,
                       block = NONE,
                       canOutput = SOME (fn () => true),
                       getPos = NONE,
                       setPos = NONE,
                       endPos = NONE,
                       verifyPos = NONE,
                       (* We should perhaps actually close. *)
                       close = (fn () => ()),
                       ioDesc = NONE
                       })
          val stream' = IOS.mkOutstream (
                        IOS.StreamIO.mkOutstream (stream, IO.NO_BUF))
      in (data, stream') end

  fun getWriterData data =
      let val vec = VS.concat (rev (!data))
          (* Update the list with the new concatenated thing *)
          val () = data := [VS.full vec]
      in vec end

  val empty_vec = V.fromList []
  fun openVector v = IOS.mkInstream (
                     IOS.StreamIO.mkInstream (Prim.openVector v, empty_vec))
end
