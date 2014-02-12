
(*#line 3.1 "syntax-io.ioml"*)signature 
(*#line 3.11 "syntax-io.ioml"*)VARIABLE  = 
(*#line 4.1 "syntax-io.ioml"*)sig 
(*#line 5.3 "syntax-io.ioml"*)
(*#line 5.3 "syntax-io.ioml"*)
(*#line 5.3 "syntax-io.ioml"*)
(*#line 5.3 "syntax-io.ioml"*)
(*#line 5.3 "syntax-io.ioml"*)
(*#line 5.3 "syntax-io.ioml"*)type 
(*#line 5.10 "syntax-io.ioml"*)var 
(*#line 12.32 "syntax-io.sml"*)val writeVar : (BinIO.outstream   * var  ) -> unit val readVar : BinIO.instream   -> var   option  
(*#line 6.3 "syntax-io.ioml"*)
(*#line 6.3 "syntax-io.ioml"*)type 
(*#line 6.10 "syntax-io.ioml"*)
(*#line 6.10 "syntax-io.ioml"*)t  = 
(*#line 6.14 "syntax-io.ioml"*)var  
(*#line 18.32 "syntax-io.sml"*)val writeT : (BinIO.outstream   * t  ) -> unit val readT : BinIO.instream   -> t   option  
(*#line 7.3 "syntax-io.ioml"*)val 
(*#line 7.7 "syntax-io.ioml"*)toStr : 
(*#line 7.15 "syntax-io.ioml"*)
(*#line 7.15 "syntax-io.ioml"*)var   -> 
(*#line 7.22 "syntax-io.ioml"*)string  
(*#line 8.3 "syntax-io.ioml"*)val 
(*#line 8.7 "syntax-io.ioml"*)eq : 
(*#line 8.12 "syntax-io.ioml"*)
(*#line 8.12 "syntax-io.ioml"*)(
(*#line 8.12 "syntax-io.ioml"*)var   * 
(*#line 8.18 "syntax-io.ioml"*)var  ) -> 
(*#line 8.25 "syntax-io.ioml"*)bool  
(*#line 9.3 "syntax-io.ioml"*)val 
(*#line 9.7 "syntax-io.ioml"*)compare : 
(*#line 9.17 "syntax-io.ioml"*)
(*#line 9.17 "syntax-io.ioml"*)(
(*#line 9.17 "syntax-io.ioml"*)var   * 
(*#line 9.23 "syntax-io.ioml"*)var  ) -> 
(*#line 9.30 "syntax-io.ioml"*)order  end 
(*#line 13.1 "syntax-io.ioml"*)
(*#line 13.1 "syntax-io.ioml"*)structure Variable: 
(*#line 13.22 "syntax-io.ioml"*)
(*#line 13.22 "syntax-io.ioml"*)VARIABLE  = 
(*#line 14.1 "syntax-io.ioml"*)struct 
(*#line 15.3 "syntax-io.ioml"*)
(*#line 15.3 "syntax-io.ioml"*)
(*#line 15.3 "syntax-io.ioml"*)
(*#line 15.3 "syntax-io.ioml"*)type 
(*#line 15.10 "syntax-io.ioml"*)
(*#line 15.10 "syntax-io.ioml"*)var  = 
(*#line 15.16 "syntax-io.ioml"*)string  
(*#line 50.32 "syntax-io.sml"*)fun writeVar (p , x : var  ) = IOTypes.writeString (p , x )fun readVar p  = IOTypes.readString p 
(*#line 16.3 "syntax-io.ioml"*)
(*#line 16.3 "syntax-io.ioml"*)
(*#line 16.3 "syntax-io.ioml"*)
(*#line 16.3 "syntax-io.ioml"*)type 
(*#line 16.10 "syntax-io.ioml"*)
(*#line 16.10 "syntax-io.ioml"*)t  = 
(*#line 16.14 "syntax-io.ioml"*)var  
(*#line 58.32 "syntax-io.sml"*)fun writeT (p , x : t  ) = writeVar (p , x )fun readT p  = readVar p 
(*#line 17.3 "syntax-io.ioml"*)
(*#line 17.3 "syntax-io.ioml"*)
(*#line 17.3 "syntax-io.ioml"*)fun 
(*#line 17.7 "syntax-io.ioml"*)toStr 
(*#line 17.13 "syntax-io.ioml"*)s  = 
(*#line 17.17 "syntax-io.ioml"*)
(*#line 17.17 "syntax-io.ioml"*)s 
(*#line 18.3 "syntax-io.ioml"*)
(*#line 18.3 "syntax-io.ioml"*)
(*#line 18.3 "syntax-io.ioml"*)fun 
(*#line 18.7 "syntax-io.ioml"*)eq 
(*#line 18.10 "syntax-io.ioml"*)(
(*#line 18.11 "syntax-io.ioml"*)
(*#line 18.11 "syntax-io.ioml"*)
(*#line 18.11 "syntax-io.ioml"*)v : 
(*#line 18.14 "syntax-io.ioml"*)var  , 
(*#line 18.19 "syntax-io.ioml"*)
(*#line 18.19 "syntax-io.ioml"*)v' ) = 
(*#line 18.25 "syntax-io.ioml"*)
(*#line 18.25 "syntax-io.ioml"*)v 
(*#line 18.27 "syntax-io.ioml"*)= 
(*#line 18.29 "syntax-io.ioml"*)v' 
(*#line 19.3 "syntax-io.ioml"*)
(*#line 19.3 "syntax-io.ioml"*)
(*#line 19.3 "syntax-io.ioml"*)val 
(*#line 19.7 "syntax-io.ioml"*)
(*#line 19.7 "syntax-io.ioml"*)compare  = 
(*#line 19.17 "syntax-io.ioml"*)
(*#line 19.17 "syntax-io.ioml"*)String.compare 
(*#line 20.1 "syntax-io.ioml"*)end 
(*#line 21.1 "syntax-io.ioml"*)
(*#line 21.1 "syntax-io.ioml"*)structure VarDict = 
(*#line 21.21 "syntax-io.ioml"*)
(*#line 21.21 "syntax-io.ioml"*)SplayDict (
(*#line 21.31 "syntax-io.ioml"*)struct 
(*#line 21.31 "syntax-io.ioml"*)
(*#line 21.31 "syntax-io.ioml"*)structure Key = 
(*#line 21.47 "syntax-io.ioml"*)Variable 
(*#line 21.55 "syntax-io.ioml"*)end ) 
(*#line 22.1 "syntax-io.ioml"*)
(*#line 22.1 "syntax-io.ioml"*)structure VarSet = 
(*#line 22.20 "syntax-io.ioml"*)
(*#line 22.20 "syntax-io.ioml"*)SplaySet (
(*#line 22.29 "syntax-io.ioml"*)struct 
(*#line 22.29 "syntax-io.ioml"*)
(*#line 22.29 "syntax-io.ioml"*)structure Elem = 
(*#line 22.46 "syntax-io.ioml"*)Variable 
(*#line 22.54 "syntax-io.ioml"*)end ) 
(*#line 25.1 "syntax-io.ioml"*)
(*#line 25.1 "syntax-io.ioml"*)structure Const = 
(*#line 26.1 "syntax-io.ioml"*)struct 
(*#line 27.3 "syntax-io.ioml"*)
(*#line 27.3 "syntax-io.ioml"*)
(*#line 27.3 "syntax-io.ioml"*)
(*#line 27.3 "syntax-io.ioml"*)type 
(*#line 27.10 "syntax-io.ioml"*)
(*#line 27.10 "syntax-io.ioml"*)namespace  = 
(*#line 27.22 "syntax-io.ioml"*)string  
(*#line 117.33 "syntax-io.sml"*)fun writeNamespace (p , x : namespace  ) = IOTypes.writeString (p , x )fun readNamespace p  = IOTypes.readString p 
(*#line 28.3 "syntax-io.ioml"*)
(*#line 28.3 "syntax-io.ioml"*)
(*#line 28.3 "syntax-io.ioml"*)
(*#line 28.14 "syntax-io.ioml"*)
(*#line 28.14 "syntax-io.ioml"*)datatype 
(*#line 28.14 "syntax-io.ioml"*)
(*#line 28.14 "syntax-io.ioml"*)location  = 
(*#line 28.25 "syntax-io.ioml"*)LThis | 
(*#line 28.33 "syntax-io.ioml"*)LId of 
(*#line 28.40 "syntax-io.ioml"*)namespace  
(*#line 128.33 "syntax-io.sml"*)fun writeLocation (p , x ) = case x of LThis  => IOTypes.writeInt (p , 0 ) | LId x  => (IOTypes.writeInt (p , 1 ); writeNamespace (p , x ))fun readLocation p  = case IOTypes.readInt p of SOME 0  => SOME LThis  | SOME 1  => (case readNamespace p of NONE  => NONE  | SOME x  => SOME (LId x )) | _  => NONE 
(*#line 29.3 "syntax-io.ioml"*)
(*#line 29.3 "syntax-io.ioml"*)
(*#line 29.3 "syntax-io.ioml"*)
(*#line 29.3 "syntax-io.ioml"*)type 
(*#line 29.10 "syntax-io.ioml"*)
(*#line 29.10 "syntax-io.ioml"*)id  = 
(*#line 29.15 "syntax-io.ioml"*)string  
(*#line 136.33 "syntax-io.sml"*)fun writeId (p , x : id  ) = IOTypes.writeString (p , x )fun readId p  = IOTypes.readString p 
(*#line 30.3 "syntax-io.ioml"*)
(*#line 30.3 "syntax-io.ioml"*)
(*#line 30.3 "syntax-io.ioml"*)
(*#line 30.3 "syntax-io.ioml"*)type 
(*#line 30.10 "syntax-io.ioml"*)
(*#line 30.10 "syntax-io.ioml"*)const  = 
(*#line 30.18 "syntax-io.ioml"*)(
(*#line 30.18 "syntax-io.ioml"*)location   * 
(*#line 30.29 "syntax-io.ioml"*)id  )
(*#line 146.33 "syntax-io.sml"*)fun writeConst (p , x : const  ) = ((fn x  => writeLocation (p , x ))(# 1 x ); (fn x  => writeId (p , x ))(# 2 x ))fun readConst p  = case readLocation p of NONE  => NONE  | SOME y0  => (case readId p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))
(*#line 31.3 "syntax-io.ioml"*)
(*#line 31.3 "syntax-io.ioml"*)
(*#line 31.3 "syntax-io.ioml"*)
(*#line 31.3 "syntax-io.ioml"*)type 
(*#line 31.10 "syntax-io.ioml"*)
(*#line 31.10 "syntax-io.ioml"*)t  = 
(*#line 31.14 "syntax-io.ioml"*)const  
(*#line 154.33 "syntax-io.sml"*)fun writeT (p , x : t  ) = writeConst (p , x )fun readT p  = readConst p 
(*#line 33.3 "syntax-io.ioml"*)
(*#line 33.3 "syntax-io.ioml"*)
(*#line 33.3 "syntax-io.ioml"*)fun 
(*#line 33.7 "syntax-io.ioml"*)toStr 
(*#line 33.13 "syntax-io.ioml"*)(
(*#line 33.14 "syntax-io.ioml"*)
(*#line 33.14 "syntax-io.ioml"*)LThis , 
(*#line 33.21 "syntax-io.ioml"*)
(*#line 33.21 "syntax-io.ioml"*)s ) = 
(*#line 33.26 "syntax-io.ioml"*)
(*#line 33.26 "syntax-io.ioml"*)s | 
(*#line 34.7 "syntax-io.ioml"*)toStr 
(*#line 34.13 "syntax-io.ioml"*)(
(*#line 34.14 "syntax-io.ioml"*)
(*#line 34.14 "syntax-io.ioml"*)LId 
(*#line 34.18 "syntax-io.ioml"*)n , 
(*#line 34.21 "syntax-io.ioml"*)
(*#line 34.21 "syntax-io.ioml"*)s ) = 
(*#line 34.26 "syntax-io.ioml"*)
(*#line 34.26 "syntax-io.ioml"*)n 
(*#line 34.28 "syntax-io.ioml"*)^ 
(*#line 34.30 "syntax-io.ioml"*)
(*#line 34.30 "syntax-io.ioml"*)"."
(*#line 34.34 "syntax-io.ioml"*)^ 
(*#line 34.36 "syntax-io.ioml"*)s 
(*#line 36.3 "syntax-io.ioml"*)
(*#line 36.3 "syntax-io.ioml"*)
(*#line 36.3 "syntax-io.ioml"*)fun 
(*#line 36.7 "syntax-io.ioml"*)eq 
(*#line 36.10 "syntax-io.ioml"*)(
(*#line 36.11 "syntax-io.ioml"*)
(*#line 36.11 "syntax-io.ioml"*)
(*#line 36.11 "syntax-io.ioml"*)x : 
(*#line 36.14 "syntax-io.ioml"*)const  , 
(*#line 36.21 "syntax-io.ioml"*)
(*#line 36.21 "syntax-io.ioml"*)y ) = 
(*#line 36.26 "syntax-io.ioml"*)
(*#line 36.26 "syntax-io.ioml"*)x 
(*#line 36.28 "syntax-io.ioml"*)= 
(*#line 36.30 "syntax-io.ioml"*)y 
(*#line 39.3 "syntax-io.ioml"*)
(*#line 39.3 "syntax-io.ioml"*)
(*#line 39.3 "syntax-io.ioml"*)fun 
(*#line 39.7 "syntax-io.ioml"*)cmp_location 
(*#line 39.20 "syntax-io.ioml"*)(
(*#line 39.21 "syntax-io.ioml"*)
(*#line 39.21 "syntax-io.ioml"*)LThis , 
(*#line 39.28 "syntax-io.ioml"*)
(*#line 39.28 "syntax-io.ioml"*)LThis ) = 
(*#line 39.37 "syntax-io.ioml"*)
(*#line 39.37 "syntax-io.ioml"*)EQUAL | 
(*#line 40.7 "syntax-io.ioml"*)cmp_location 
(*#line 40.20 "syntax-io.ioml"*)(
(*#line 40.21 "syntax-io.ioml"*)
(*#line 40.21 "syntax-io.ioml"*)LId 
(*#line 40.25 "syntax-io.ioml"*)s1 , 
(*#line 40.29 "syntax-io.ioml"*)
(*#line 40.29 "syntax-io.ioml"*)LId 
(*#line 40.33 "syntax-io.ioml"*)s2 ) = 
(*#line 40.39 "syntax-io.ioml"*)
(*#line 40.39 "syntax-io.ioml"*)String.compare 
(*#line 40.54 "syntax-io.ioml"*)(
(*#line 40.55 "syntax-io.ioml"*)
(*#line 40.55 "syntax-io.ioml"*)s1 , 
(*#line 40.59 "syntax-io.ioml"*)
(*#line 40.59 "syntax-io.ioml"*)s2 )| 
(*#line 41.7 "syntax-io.ioml"*)cmp_location 
(*#line 41.20 "syntax-io.ioml"*)(
(*#line 41.21 "syntax-io.ioml"*)
(*#line 41.21 "syntax-io.ioml"*)LThis , 
(*#line 41.28 "syntax-io.ioml"*)
(*#line 41.28 "syntax-io.ioml"*)LId 
(*#line 41.32 "syntax-io.ioml"*)_ ) = 
(*#line 41.37 "syntax-io.ioml"*)
(*#line 41.37 "syntax-io.ioml"*)LESS | 
(*#line 42.7 "syntax-io.ioml"*)cmp_location 
(*#line 42.20 "syntax-io.ioml"*)(
(*#line 42.21 "syntax-io.ioml"*)
(*#line 42.21 "syntax-io.ioml"*)LId 
(*#line 42.25 "syntax-io.ioml"*)_ , 
(*#line 42.28 "syntax-io.ioml"*)
(*#line 42.28 "syntax-io.ioml"*)LThis ) = 
(*#line 42.37 "syntax-io.ioml"*)
(*#line 42.37 "syntax-io.ioml"*)GREATER 
(*#line 43.3 "syntax-io.ioml"*)
(*#line 43.3 "syntax-io.ioml"*)
(*#line 43.3 "syntax-io.ioml"*)fun 
(*#line 43.7 "syntax-io.ioml"*)compare 
(*#line 43.15 "syntax-io.ioml"*)(
(*#line 43.16 "syntax-io.ioml"*)
(*#line 43.16 "syntax-io.ioml"*)(
(*#line 43.17 "syntax-io.ioml"*)
(*#line 43.17 "syntax-io.ioml"*)l1 , 
(*#line 43.21 "syntax-io.ioml"*)
(*#line 43.21 "syntax-io.ioml"*)s1 ), 
(*#line 43.26 "syntax-io.ioml"*)
(*#line 43.26 "syntax-io.ioml"*)(
(*#line 43.27 "syntax-io.ioml"*)
(*#line 43.27 "syntax-io.ioml"*)l2 , 
(*#line 43.31 "syntax-io.ioml"*)
(*#line 43.31 "syntax-io.ioml"*)s2 )) = 
(*#line 44.7 "syntax-io.ioml"*)
(*#line 44.7 "syntax-io.ioml"*)(
(*#line 44.8 "syntax-io.ioml"*)case 
(*#line 44.13 "syntax-io.ioml"*)
(*#line 44.13 "syntax-io.ioml"*)cmp_location 
(*#line 44.26 "syntax-io.ioml"*)(
(*#line 44.27 "syntax-io.ioml"*)
(*#line 44.27 "syntax-io.ioml"*)l1 , 
(*#line 44.31 "syntax-io.ioml"*)
(*#line 44.31 "syntax-io.ioml"*)l2 )of 
(*#line 45.12 "syntax-io.ioml"*)
(*#line 45.12 "syntax-io.ioml"*)
(*#line 45.12 "syntax-io.ioml"*)EQUAL  => 
(*#line 45.21 "syntax-io.ioml"*)
(*#line 45.21 "syntax-io.ioml"*)String.compare 
(*#line 45.36 "syntax-io.ioml"*)(
(*#line 45.37 "syntax-io.ioml"*)
(*#line 45.37 "syntax-io.ioml"*)s1 , 
(*#line 45.41 "syntax-io.ioml"*)
(*#line 45.41 "syntax-io.ioml"*)s2 ) | 
(*#line 46.12 "syntax-io.ioml"*)
(*#line 46.12 "syntax-io.ioml"*)x  => 
(*#line 46.17 "syntax-io.ioml"*)
(*#line 46.17 "syntax-io.ioml"*)x )
(*#line 48.1 "syntax-io.ioml"*)end 
(*#line 50.1 "syntax-io.ioml"*)
(*#line 50.1 "syntax-io.ioml"*)structure ConstDict = 
(*#line 50.23 "syntax-io.ioml"*)
(*#line 50.23 "syntax-io.ioml"*)SplayDict (
(*#line 50.33 "syntax-io.ioml"*)struct 
(*#line 50.33 "syntax-io.ioml"*)
(*#line 50.33 "syntax-io.ioml"*)structure Key = 
(*#line 50.49 "syntax-io.ioml"*)Const 
(*#line 50.54 "syntax-io.ioml"*)end ) 
(*#line 51.1 "syntax-io.ioml"*)
(*#line 51.1 "syntax-io.ioml"*)structure ConstSet = 
(*#line 51.22 "syntax-io.ioml"*)
(*#line 51.22 "syntax-io.ioml"*)SplaySet (
(*#line 51.31 "syntax-io.ioml"*)struct 
(*#line 51.31 "syntax-io.ioml"*)
(*#line 51.31 "syntax-io.ioml"*)structure Elem = 
(*#line 51.48 "syntax-io.ioml"*)Const 
(*#line 51.53 "syntax-io.ioml"*)end ) 
(*#line 54.1 "syntax-io.ioml"*)
(*#line 54.1 "syntax-io.ioml"*)structure LFSyntax = 
(*#line 55.1 "syntax-io.ioml"*)struct 
(*#line 57.3 "syntax-io.ioml"*)
(*#line 57.3 "syntax-io.ioml"*)
(*#line 57.3 "syntax-io.ioml"*)
(*#line 57.3 "syntax-io.ioml"*)type 
(*#line 57.10 "syntax-io.ioml"*)
(*#line 57.10 "syntax-io.ioml"*)var  = 
(*#line 57.16 "syntax-io.ioml"*)(
(*#line 57.16 "syntax-io.ioml"*)int   * 
(*#line 57.22 "syntax-io.ioml"*)string  )
(*#line 311.33 "syntax-io.sml"*)fun writeVar (p , x : var  ) = ((fn x  => IOTypes.writeInt (p , x ))(# 1 x ); (fn x  => IOTypes.writeString (p , x ))(# 2 x ))fun readVar p  = case IOTypes.readInt p of NONE  => NONE  | SOME y0  => (case IOTypes.readString p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))
(*#line 58.3 "syntax-io.ioml"*)
(*#line 58.3 "syntax-io.ioml"*)
(*#line 58.3 "syntax-io.ioml"*)
(*#line 58.3 "syntax-io.ioml"*)type 
(*#line 58.10 "syntax-io.ioml"*)
(*#line 58.10 "syntax-io.ioml"*)const  = 
(*#line 58.18 "syntax-io.ioml"*)Const.const  
(*#line 319.33 "syntax-io.sml"*)fun writeConst (p , x : const  ) = 
(*#line 58.18 "syntax-io.ioml"*)
(*#line 58.18 "syntax-io.ioml"*)Const.writeConst 
(*#line 322.33 "syntax-io.sml"*)(p , x )fun readConst p  = 
(*#line 58.18 "syntax-io.ioml"*)
(*#line 58.18 "syntax-io.ioml"*)Const.readConst 
(*#line 325.33 "syntax-io.sml"*)p 
(*#line 59.3 "syntax-io.ioml"*)
(*#line 59.3 "syntax-io.ioml"*)
(*#line 59.3 "syntax-io.ioml"*)
(*#line 59.3 "syntax-io.ioml"*)type 
(*#line 59.10 "syntax-io.ioml"*)
(*#line 59.10 "syntax-io.ioml"*)binding  = 
(*#line 59.20 "syntax-io.ioml"*)string  
(*#line 333.33 "syntax-io.sml"*)fun writeBinding (p , x : binding  ) = IOTypes.writeString (p , x )fun readBinding p  = IOTypes.readString p 
(*#line 62.3 "syntax-io.ioml"*)
(*#line 62.3 "syntax-io.ioml"*)
(*#line 62.3 "syntax-io.ioml"*)
(*#line 62.14 "syntax-io.ioml"*)
(*#line 62.14 "syntax-io.ioml"*)datatype 
(*#line 62.14 "syntax-io.ioml"*)
(*#line 62.14 "syntax-io.ioml"*)head  = 
(*#line 62.21 "syntax-io.ioml"*)HVar of 
(*#line 62.29 "syntax-io.ioml"*)var  | 
(*#line 63.19 "syntax-io.ioml"*)HConst of 
(*#line 63.29 "syntax-io.ioml"*)const  
(*#line 345.33 "syntax-io.sml"*)fun writeHead (p , x ) = case x of HVar x  => (IOTypes.writeInt (p , 0 ); writeVar (p , x )) | HConst x  => (IOTypes.writeInt (p , 1 ); writeConst (p , x ))fun readHead p  = case IOTypes.readInt p of SOME 0  => (case readVar p of NONE  => NONE  | SOME x  => SOME (HVar x )) | SOME 1  => (case readConst p of NONE  => NONE  | SOME x  => SOME (HConst x )) | _  => NONE 
(*#line 65.3 "syntax-io.ioml"*)
(*#line 65.3 "syntax-io.ioml"*)
(*#line 65.3 "syntax-io.ioml"*)
(*#line 65.14 "syntax-io.ioml"*)
(*#line 65.14 "syntax-io.ioml"*)datatype 
(*#line 65.14 "syntax-io.ioml"*)
(*#line 65.14 "syntax-io.ioml"*)exp  = 
(*#line 65.20 "syntax-io.ioml"*)EKind | 
(*#line 66.18 "syntax-io.ioml"*)EType | 
(*#line 67.18 "syntax-io.ioml"*)EProp | 
(*#line 68.18 "syntax-io.ioml"*)EPi of 
(*#line 68.25 "syntax-io.ioml"*)(
(*#line 68.25 "syntax-io.ioml"*)binding   * 
(*#line 68.35 "syntax-io.ioml"*)exp   * 
(*#line 68.41 "syntax-io.ioml"*)exp  )| 
(*#line 69.18 "syntax-io.ioml"*)ELam of 
(*#line 69.26 "syntax-io.ioml"*)(
(*#line 69.26 "syntax-io.ioml"*)binding   * 
(*#line 69.36 "syntax-io.ioml"*)exp  )| 
(*#line 70.18 "syntax-io.ioml"*)EApp of 
(*#line 70.26 "syntax-io.ioml"*)(
(*#line 70.26 "syntax-io.ioml"*)head   * 
(*#line 70.33 "syntax-io.ioml"*)spine  )and 
(*#line 72.12 "syntax-io.ioml"*)spine  = 
(*#line 72.20 "syntax-io.ioml"*)SNil | 
(*#line 73.20 "syntax-io.ioml"*)SApp of 
(*#line 73.28 "syntax-io.ioml"*)(
(*#line 73.28 "syntax-io.ioml"*)exp   * 
(*#line 73.34 "syntax-io.ioml"*)spine  )
(*#line 375.33 "syntax-io.sml"*)fun writeExp (p , x ) = case x of EKind  => IOTypes.writeInt (p , 0 ) | EType  => IOTypes.writeInt (p , 1 ) | EProp  => IOTypes.writeInt (p , 2 ) | EPi x  => (IOTypes.writeInt (p , 3 ); ((fn x  => writeBinding (p , x ))(# 1 x ); (fn x  => writeExp (p , x ))(# 2 x ); (fn x  => writeExp (p , x ))(# 3 x ))) | ELam x  => (IOTypes.writeInt (p , 4 ); ((fn x  => writeBinding (p , x ))(# 1 x ); (fn x  => writeExp (p , x ))(# 2 x ))) | EApp x  => (IOTypes.writeInt (p , 5 ); ((fn x  => writeHead (p , x ))(# 1 x ); (fn x  => writeSpine (p , x ))(# 2 x )))and writeSpine (p , x ) = case x of SNil  => IOTypes.writeInt (p , 0 ) | SApp x  => (IOTypes.writeInt (p , 1 ); ((fn x  => writeExp (p , x ))(# 1 x ); (fn x  => writeSpine (p , x ))(# 2 x )))fun readExp p  = case IOTypes.readInt p of SOME 0  => SOME EKind  | SOME 1  => SOME EType  | SOME 2  => SOME EProp  | SOME 3  => (case case readBinding p of NONE  => NONE  | SOME y0  => (case readExp p of NONE  => NONE  | SOME y1  => (case readExp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (EPi x )) | SOME 4  => (case case readBinding p of NONE  => NONE  | SOME y0  => (case readExp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (ELam x )) | SOME 5  => (case case readHead p of NONE  => NONE  | SOME y0  => (case readSpine p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (EApp x )) | _  => NONE and readSpine p  = case IOTypes.readInt p of SOME 0  => SOME SNil  | SOME 1  => (case case readExp p of NONE  => NONE  | SOME y0  => (case readSpine p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (SApp x )) | _  => NONE 
(*#line 75.3 "syntax-io.ioml"*)
(*#line 75.3 "syntax-io.ioml"*)
(*#line 75.3 "syntax-io.ioml"*)
(*#line 75.14 "syntax-io.ioml"*)
(*#line 75.14 "syntax-io.ioml"*)datatype 
(*#line 75.14 "syntax-io.ioml"*)
(*#line 75.14 "syntax-io.ioml"*)entry_type  = 
(*#line 75.27 "syntax-io.ioml"*)BasisFamilyDecl | 
(*#line 75.45 "syntax-io.ioml"*)BasisObjectDecl 
(*#line 385.33 "syntax-io.sml"*)fun writeEntry_type (p , x ) = case x of BasisFamilyDecl  => IOTypes.writeInt (p , 0 ) | BasisObjectDecl  => IOTypes.writeInt (p , 1 )fun readEntry_type p  = case IOTypes.readInt p of SOME 0  => SOME BasisFamilyDecl  | SOME 1  => SOME BasisObjectDecl  | _  => NONE 
(*#line 76.3 "syntax-io.ioml"*)
(*#line 76.3 "syntax-io.ioml"*)
(*#line 76.3 "syntax-io.ioml"*)
(*#line 76.3 "syntax-io.ioml"*)type 
(*#line 76.10 "syntax-io.ioml"*)
(*#line 76.10 "syntax-io.ioml"*)basis_entry  = 
(*#line 76.24 "syntax-io.ioml"*)(
(*#line 76.24 "syntax-io.ioml"*)entry_type   * 
(*#line 76.37 "syntax-io.ioml"*)Const.id   * 
(*#line 76.48 "syntax-io.ioml"*)exp  )
(*#line 396.33 "syntax-io.sml"*)fun writeBasis_entry (p , x : basis_entry  ) = ((fn x  => writeEntry_type (p , x ))(# 1 x ); 
(*#line 76.37 "syntax-io.ioml"*)
(*#line 76.37 "syntax-io.ioml"*)(
(*#line 76.37 "syntax-io.ioml"*)fn 
(*#line 76.37 "syntax-io.ioml"*)
(*#line 401.33 "syntax-io.sml"*)x  => 
(*#line 76.37 "syntax-io.ioml"*)
(*#line 76.37 "syntax-io.ioml"*)Const.writeId 
(*#line 404.33 "syntax-io.sml"*)(p , x ))(# 2 x ); (fn x  => writeExp (p , x ))(# 3 x ))fun readBasis_entry p  = case readEntry_type p of NONE  => NONE  | SOME y0  => (case 
(*#line 76.37 "syntax-io.ioml"*)
(*#line 76.37 "syntax-io.ioml"*)Const.readId 
(*#line 407.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (case readExp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))
(*#line 77.3 "syntax-io.ioml"*)
(*#line 77.3 "syntax-io.ioml"*)
(*#line 77.3 "syntax-io.ioml"*)
(*#line 77.3 "syntax-io.ioml"*)type 
(*#line 77.10 "syntax-io.ioml"*)
(*#line 77.10 "syntax-io.ioml"*)basis  = 
(*#line 77.18 "syntax-io.ioml"*)
(*#line 77.18 "syntax-io.ioml"*)basis_entry   list  
(*#line 416.33 "syntax-io.sml"*)fun writeBasis (p , x : basis  ) = IOTypes.writeList (writeBasis_entry )(p , x )fun readBasis p  = IOTypes.readList (readBasis_entry )p 
(*#line 79.3 "syntax-io.ioml"*)
(*#line 79.3 "syntax-io.ioml"*)
(*#line 79.3 "syntax-io.ioml"*)val 
(*#line 79.7 "syntax-io.ioml"*)
(*#line 79.7 "syntax-io.ioml"*)listToSpine  = 
(*#line 79.21 "syntax-io.ioml"*)
(*#line 79.21 "syntax-io.ioml"*)foldr 
(*#line 79.27 "syntax-io.ioml"*)SApp 
(*#line 79.32 "syntax-io.ioml"*)SNil 
(*#line 80.3 "syntax-io.ioml"*)
(*#line 80.3 "syntax-io.ioml"*)
(*#line 80.3 "syntax-io.ioml"*)fun 
(*#line 80.7 "syntax-io.ioml"*)spineToList 
(*#line 80.19 "syntax-io.ioml"*)SNil  = 
(*#line 80.26 "syntax-io.ioml"*)
(*#line 80.26 "syntax-io.ioml"*)nil | 
(*#line 81.7 "syntax-io.ioml"*)spineToList 
(*#line 81.19 "syntax-io.ioml"*)(
(*#line 81.20 "syntax-io.ioml"*)
(*#line 81.20 "syntax-io.ioml"*)SApp 
(*#line 81.25 "syntax-io.ioml"*)(
(*#line 81.26 "syntax-io.ioml"*)
(*#line 81.26 "syntax-io.ioml"*)e , 
(*#line 81.29 "syntax-io.ioml"*)
(*#line 81.29 "syntax-io.ioml"*)s )) = 
(*#line 81.35 "syntax-io.ioml"*)
(*#line 81.35 "syntax-io.ioml"*)e 
(*#line 81.37 "syntax-io.ioml"*):: 
(*#line 81.40 "syntax-io.ioml"*)spineToList 
(*#line 81.52 "syntax-io.ioml"*)s 
(*#line 83.3 "syntax-io.ioml"*)
(*#line 83.3 "syntax-io.ioml"*)
(*#line 83.3 "syntax-io.ioml"*)fun 
(*#line 83.7 "syntax-io.ioml"*)mapSpine 
(*#line 83.16 "syntax-io.ioml"*)f  = 
(*#line 83.20 "syntax-io.ioml"*)
(*#line 83.20 "syntax-io.ioml"*)listToSpine 
(*#line 83.32 "syntax-io.ioml"*)o 
(*#line 83.34 "syntax-io.ioml"*)map 
(*#line 83.38 "syntax-io.ioml"*)f 
(*#line 83.40 "syntax-io.ioml"*)o 
(*#line 83.42 "syntax-io.ioml"*)spineToList 
(*#line 85.1 "syntax-io.ioml"*)end 
(*#line 86.1 "syntax-io.ioml"*)
(*#line 86.1 "syntax-io.ioml"*)structure LF = 
(*#line 86.16 "syntax-io.ioml"*)LFSyntax 
(*#line 88.1 "syntax-io.ioml"*)
(*#line 88.1 "syntax-io.ioml"*)structure Logic = 
(*#line 89.1 "syntax-io.ioml"*)struct 
(*#line 91.3 "syntax-io.ioml"*)
(*#line 91.3 "syntax-io.ioml"*)structure LF = 
(*#line 91.18 "syntax-io.ioml"*)LFSyntax 
(*#line 94.3 "syntax-io.ioml"*)
(*#line 94.3 "syntax-io.ioml"*)
(*#line 94.3 "syntax-io.ioml"*)
(*#line 94.3 "syntax-io.ioml"*)type 
(*#line 94.10 "syntax-io.ioml"*)
(*#line 94.10 "syntax-io.ioml"*)atom  = 
(*#line 94.17 "syntax-io.ioml"*)LF.exp  
(*#line 476.33 "syntax-io.sml"*)fun writeAtom (p , x : atom  ) = 
(*#line 94.17 "syntax-io.ioml"*)
(*#line 94.17 "syntax-io.ioml"*)LF.writeExp 
(*#line 479.33 "syntax-io.sml"*)(p , x )fun readAtom p  = 
(*#line 94.17 "syntax-io.ioml"*)
(*#line 94.17 "syntax-io.ioml"*)LF.readExp 
(*#line 482.33 "syntax-io.sml"*)p 
(*#line 96.3 "syntax-io.ioml"*)
(*#line 96.3 "syntax-io.ioml"*)
(*#line 96.3 "syntax-io.ioml"*)
(*#line 96.3 "syntax-io.ioml"*)type 
(*#line 96.10 "syntax-io.ioml"*)
(*#line 96.10 "syntax-io.ioml"*)principal  = 
(*#line 96.22 "syntax-io.ioml"*)LF.exp  
(*#line 490.33 "syntax-io.sml"*)fun writePrincipal (p , x : principal  ) = 
(*#line 96.22 "syntax-io.ioml"*)
(*#line 96.22 "syntax-io.ioml"*)LF.writeExp 
(*#line 493.33 "syntax-io.sml"*)(p , x )fun readPrincipal p  = 
(*#line 96.22 "syntax-io.ioml"*)
(*#line 96.22 "syntax-io.ioml"*)LF.readExp 
(*#line 496.33 "syntax-io.sml"*)p 
(*#line 98.3 "syntax-io.ioml"*)
(*#line 98.3 "syntax-io.ioml"*)
(*#line 98.3 "syntax-io.ioml"*)
(*#line 98.3 "syntax-io.ioml"*)type 
(*#line 98.10 "syntax-io.ioml"*)
(*#line 98.10 "syntax-io.ioml"*)address  = 
(*#line 98.20 "syntax-io.ioml"*)LF.exp  
(*#line 504.33 "syntax-io.sml"*)fun writeAddress (p , x : address  ) = 
(*#line 98.20 "syntax-io.ioml"*)
(*#line 98.20 "syntax-io.ioml"*)LF.writeExp 
(*#line 507.33 "syntax-io.sml"*)(p , x )fun readAddress p  = 
(*#line 98.20 "syntax-io.ioml"*)
(*#line 98.20 "syntax-io.ioml"*)LF.readExp 
(*#line 510.33 "syntax-io.sml"*)p 
(*#line 100.3 "syntax-io.ioml"*)
(*#line 100.3 "syntax-io.ioml"*)
(*#line 100.3 "syntax-io.ioml"*)
(*#line 100.3 "syntax-io.ioml"*)type 
(*#line 100.10 "syntax-io.ioml"*)
(*#line 100.10 "syntax-io.ioml"*)number  = 
(*#line 100.19 "syntax-io.ioml"*)LF.exp  
(*#line 518.33 "syntax-io.sml"*)fun writeNumber (p , x : number  ) = 
(*#line 100.19 "syntax-io.ioml"*)
(*#line 100.19 "syntax-io.ioml"*)LF.writeExp 
(*#line 521.33 "syntax-io.sml"*)(p , x )fun readNumber p  = 
(*#line 100.19 "syntax-io.ioml"*)
(*#line 100.19 "syntax-io.ioml"*)LF.readExp 
(*#line 524.33 "syntax-io.sml"*)p 
(*#line 102.3 "syntax-io.ioml"*)
(*#line 102.3 "syntax-io.ioml"*)
(*#line 102.3 "syntax-io.ioml"*)
(*#line 102.3 "syntax-io.ioml"*)type 
(*#line 102.10 "syntax-io.ioml"*)
(*#line 102.10 "syntax-io.ioml"*)coord  = 
(*#line 102.18 "syntax-io.ioml"*)LF.exp  
(*#line 532.33 "syntax-io.sml"*)fun writeCoord (p , x : coord  ) = 
(*#line 102.18 "syntax-io.ioml"*)
(*#line 102.18 "syntax-io.ioml"*)LF.writeExp 
(*#line 535.33 "syntax-io.sml"*)(p , x )fun readCoord p  = 
(*#line 102.18 "syntax-io.ioml"*)
(*#line 102.18 "syntax-io.ioml"*)LF.readExp 
(*#line 538.33 "syntax-io.sml"*)p 
(*#line 104.3 "syntax-io.ioml"*)
(*#line 104.3 "syntax-io.ioml"*)
(*#line 104.3 "syntax-io.ioml"*)
(*#line 104.3 "syntax-io.ioml"*)type 
(*#line 104.10 "syntax-io.ioml"*)
(*#line 104.10 "syntax-io.ioml"*)const  = 
(*#line 104.18 "syntax-io.ioml"*)Const.const  
(*#line 546.33 "syntax-io.sml"*)fun writeConst (p , x : const  ) = 
(*#line 104.18 "syntax-io.ioml"*)
(*#line 104.18 "syntax-io.ioml"*)Const.writeConst 
(*#line 549.33 "syntax-io.sml"*)(p , x )fun readConst p  = 
(*#line 104.18 "syntax-io.ioml"*)
(*#line 104.18 "syntax-io.ioml"*)Const.readConst 
(*#line 552.33 "syntax-io.sml"*)p 
(*#line 105.3 "syntax-io.ioml"*)
(*#line 105.3 "syntax-io.ioml"*)
(*#line 105.3 "syntax-io.ioml"*)
(*#line 105.3 "syntax-io.ioml"*)type 
(*#line 105.10 "syntax-io.ioml"*)
(*#line 105.10 "syntax-io.ioml"*)var  = 
(*#line 105.16 "syntax-io.ioml"*)Variable.var  
(*#line 560.33 "syntax-io.sml"*)fun writeVar (p , x : var  ) = 
(*#line 105.16 "syntax-io.ioml"*)
(*#line 105.16 "syntax-io.ioml"*)Variable.writeVar 
(*#line 563.33 "syntax-io.sml"*)(p , x )fun readVar p  = 
(*#line 105.16 "syntax-io.ioml"*)
(*#line 105.16 "syntax-io.ioml"*)Variable.readVar 
(*#line 566.33 "syntax-io.sml"*)p 
(*#line 107.3 "syntax-io.ioml"*)
(*#line 107.3 "syntax-io.ioml"*)
(*#line 107.3 "syntax-io.ioml"*)
(*#line 107.14 "syntax-io.ioml"*)
(*#line 107.14 "syntax-io.ioml"*)datatype 
(*#line 107.14 "syntax-io.ioml"*)
(*#line 107.14 "syntax-io.ioml"*)condition  = 
(*#line 107.26 "syntax-io.ioml"*)CBefore of 
(*#line 107.37 "syntax-io.ioml"*)number  | 
(*#line 108.24 "syntax-io.ioml"*)CSpent of 
(*#line 108.34 "syntax-io.ioml"*)coord  | 
(*#line 109.24 "syntax-io.ioml"*)CTrue | 
(*#line 110.24 "syntax-io.ioml"*)CAnd of 
(*#line 110.32 "syntax-io.ioml"*)(
(*#line 110.32 "syntax-io.ioml"*)condition   * 
(*#line 110.44 "syntax-io.ioml"*)condition  )| 
(*#line 111.24 "syntax-io.ioml"*)CNot of 
(*#line 111.32 "syntax-io.ioml"*)condition  
(*#line 585.33 "syntax-io.sml"*)fun writeCondition (p , x ) = case x of CBefore x  => (IOTypes.writeInt (p , 0 ); writeNumber (p , x )) | CSpent x  => (IOTypes.writeInt (p , 1 ); writeCoord (p , x )) | CTrue  => IOTypes.writeInt (p , 2 ) | CAnd x  => (IOTypes.writeInt (p , 3 ); ((fn x  => writeCondition (p , x ))(# 1 x ); (fn x  => writeCondition (p , x ))(# 2 x ))) | CNot x  => (IOTypes.writeInt (p , 4 ); writeCondition (p , x ))fun readCondition p  = case IOTypes.readInt p of SOME 0  => (case readNumber p of NONE  => NONE  | SOME x  => SOME (CBefore x )) | SOME 1  => (case readCoord p of NONE  => NONE  | SOME x  => SOME (CSpent x )) | SOME 2  => SOME CTrue  | SOME 3  => (case case readCondition p of NONE  => NONE  | SOME y0  => (case readCondition p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (CAnd x )) | SOME 4  => (case readCondition p of NONE  => NONE  | SOME x  => SOME (CNot x )) | _  => NONE 
(*#line 114.3 "syntax-io.ioml"*)
(*#line 114.3 "syntax-io.ioml"*)
(*#line 114.3 "syntax-io.ioml"*)
(*#line 114.14 "syntax-io.ioml"*)
(*#line 114.14 "syntax-io.ioml"*)datatype 
(*#line 114.14 "syntax-io.ioml"*)
(*#line 114.14 "syntax-io.ioml"*)prop  = 
(*#line 114.21 "syntax-io.ioml"*)PAtom of 
(*#line 114.30 "syntax-io.ioml"*)atom  | 
(*#line 115.19 "syntax-io.ioml"*)PBang of 
(*#line 115.28 "syntax-io.ioml"*)prop  | 
(*#line 116.19 "syntax-io.ioml"*)PLolli of 
(*#line 116.29 "syntax-io.ioml"*)(
(*#line 116.29 "syntax-io.ioml"*)prop   * 
(*#line 116.36 "syntax-io.ioml"*)prop  )| 
(*#line 117.19 "syntax-io.ioml"*)PTensor of 
(*#line 117.30 "syntax-io.ioml"*)(
(*#line 117.30 "syntax-io.ioml"*)prop   * 
(*#line 117.37 "syntax-io.ioml"*)prop  )| 
(*#line 118.19 "syntax-io.ioml"*)PWith of 
(*#line 118.28 "syntax-io.ioml"*)(
(*#line 118.28 "syntax-io.ioml"*)prop   * 
(*#line 118.35 "syntax-io.ioml"*)prop  )| 
(*#line 119.19 "syntax-io.ioml"*)POplus of 
(*#line 119.29 "syntax-io.ioml"*)(
(*#line 119.29 "syntax-io.ioml"*)prop   * 
(*#line 119.36 "syntax-io.ioml"*)prop  )| 
(*#line 120.19 "syntax-io.ioml"*)POne | 
(*#line 121.19 "syntax-io.ioml"*)PZero | 
(*#line 123.19 "syntax-io.ioml"*)PForall of 
(*#line 123.30 "syntax-io.ioml"*)(
(*#line 123.30 "syntax-io.ioml"*)LF.binding   * 
(*#line 123.43 "syntax-io.ioml"*)LF.exp   * 
(*#line 123.52 "syntax-io.ioml"*)prop  )| 
(*#line 124.19 "syntax-io.ioml"*)PExists of 
(*#line 124.30 "syntax-io.ioml"*)(
(*#line 124.30 "syntax-io.ioml"*)LF.binding   * 
(*#line 124.43 "syntax-io.ioml"*)LF.exp   * 
(*#line 124.52 "syntax-io.ioml"*)prop  )| 
(*#line 126.19 "syntax-io.ioml"*)PAffirms of 
(*#line 126.31 "syntax-io.ioml"*)(
(*#line 126.31 "syntax-io.ioml"*)principal   * 
(*#line 126.43 "syntax-io.ioml"*)prop  )| 
(*#line 128.19 "syntax-io.ioml"*)PIf of 
(*#line 128.26 "syntax-io.ioml"*)(
(*#line 128.26 "syntax-io.ioml"*)condition   * 
(*#line 128.38 "syntax-io.ioml"*)prop  )| 
(*#line 132.19 "syntax-io.ioml"*)PReceipt of 
(*#line 132.31 "syntax-io.ioml"*)(
(*#line 132.31 "syntax-io.ioml"*)address   * 
(*#line 132.41 "syntax-io.ioml"*)prop  )
(*#line 637.33 "syntax-io.sml"*)fun writeProp (p , x ) = case x of PAtom x  => (IOTypes.writeInt (p , 0 ); writeAtom (p , x )) | PBang x  => (IOTypes.writeInt (p , 1 ); writeProp (p , x )) | PLolli x  => (IOTypes.writeInt (p , 2 ); ((fn x  => writeProp (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | PTensor x  => (IOTypes.writeInt (p , 3 ); ((fn x  => writeProp (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | PWith x  => (IOTypes.writeInt (p , 4 ); ((fn x  => writeProp (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | POplus x  => (IOTypes.writeInt (p , 5 ); ((fn x  => writeProp (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | POne  => IOTypes.writeInt (p , 6 ) | PZero  => IOTypes.writeInt (p , 7 ) | PForall x  => (IOTypes.writeInt (p , 8 ); (
(*#line 123.30 "syntax-io.ioml"*)
(*#line 123.30 "syntax-io.ioml"*)(
(*#line 123.30 "syntax-io.ioml"*)fn 
(*#line 123.30 "syntax-io.ioml"*)
(*#line 642.33 "syntax-io.sml"*)x  => 
(*#line 123.30 "syntax-io.ioml"*)
(*#line 123.30 "syntax-io.ioml"*)LF.writeBinding 
(*#line 645.33 "syntax-io.sml"*)(p , x ))(# 1 x ); 
(*#line 123.43 "syntax-io.ioml"*)
(*#line 123.43 "syntax-io.ioml"*)(
(*#line 123.43 "syntax-io.ioml"*)fn 
(*#line 123.43 "syntax-io.ioml"*)
(*#line 650.33 "syntax-io.sml"*)x  => 
(*#line 123.43 "syntax-io.ioml"*)
(*#line 123.43 "syntax-io.ioml"*)LF.writeExp 
(*#line 653.33 "syntax-io.sml"*)(p , x ))(# 2 x ); (fn x  => writeProp (p , x ))(# 3 x ))) | PExists x  => (IOTypes.writeInt (p , 9 ); (
(*#line 124.30 "syntax-io.ioml"*)
(*#line 124.30 "syntax-io.ioml"*)(
(*#line 124.30 "syntax-io.ioml"*)fn 
(*#line 124.30 "syntax-io.ioml"*)
(*#line 658.33 "syntax-io.sml"*)x  => 
(*#line 124.30 "syntax-io.ioml"*)
(*#line 124.30 "syntax-io.ioml"*)LF.writeBinding 
(*#line 661.33 "syntax-io.sml"*)(p , x ))(# 1 x ); 
(*#line 124.43 "syntax-io.ioml"*)
(*#line 124.43 "syntax-io.ioml"*)(
(*#line 124.43 "syntax-io.ioml"*)fn 
(*#line 124.43 "syntax-io.ioml"*)
(*#line 666.33 "syntax-io.sml"*)x  => 
(*#line 124.43 "syntax-io.ioml"*)
(*#line 124.43 "syntax-io.ioml"*)LF.writeExp 
(*#line 669.33 "syntax-io.sml"*)(p , x ))(# 2 x ); (fn x  => writeProp (p , x ))(# 3 x ))) | PAffirms x  => (IOTypes.writeInt (p , 10 ); ((fn x  => writePrincipal (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | PIf x  => (IOTypes.writeInt (p , 11 ); ((fn x  => writeCondition (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | PReceipt x  => (IOTypes.writeInt (p , 12 ); ((fn x  => writeAddress (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x )))fun readProp p  = case IOTypes.readInt p of SOME 0  => (case readAtom p of NONE  => NONE  | SOME x  => SOME (PAtom x )) | SOME 1  => (case readProp p of NONE  => NONE  | SOME x  => SOME (PBang x )) | SOME 2  => (case case readProp p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PLolli x )) | SOME 3  => (case case readProp p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PTensor x )) | SOME 4  => (case case readProp p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PWith x )) | SOME 5  => (case case readProp p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (POplus x )) | SOME 6  => SOME POne  | SOME 7  => SOME PZero  | SOME 8  => (case case 
(*#line 123.30 "syntax-io.ioml"*)
(*#line 123.30 "syntax-io.ioml"*)LF.readBinding 
(*#line 672.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case 
(*#line 123.43 "syntax-io.ioml"*)
(*#line 123.43 "syntax-io.ioml"*)LF.readExp 
(*#line 675.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (case readProp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (PForall x )) | SOME 9  => (case case 
(*#line 124.30 "syntax-io.ioml"*)
(*#line 124.30 "syntax-io.ioml"*)LF.readBinding 
(*#line 678.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case 
(*#line 124.43 "syntax-io.ioml"*)
(*#line 124.43 "syntax-io.ioml"*)LF.readExp 
(*#line 681.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (case readProp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (PExists x )) | SOME 10  => (case case readPrincipal p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PAffirms x )) | SOME 11  => (case case readCondition p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PIf x )) | SOME 12  => (case case readAddress p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (PReceipt x )) | _  => NONE 
(*#line 135.3 "syntax-io.ioml"*)
(*#line 135.3 "syntax-io.ioml"*)
(*#line 135.3 "syntax-io.ioml"*)
(*#line 135.14 "syntax-io.ioml"*)
(*#line 135.14 "syntax-io.ioml"*)datatype 
(*#line 135.14 "syntax-io.ioml"*)
(*#line 135.14 "syntax-io.ioml"*)idx  = 
(*#line 135.20 "syntax-io.ioml"*)L | 
(*#line 135.24 "syntax-io.ioml"*)R 
(*#line 691.33 "syntax-io.sml"*)fun writeIdx (p , x ) = case x of L  => IOTypes.writeInt (p , 0 ) | R  => IOTypes.writeInt (p , 1 )fun readIdx p  = case IOTypes.readInt p of SOME 0  => SOME L  | SOME 1  => SOME R  | _  => NONE 
(*#line 139.3 "syntax-io.ioml"*)
(*#line 139.3 "syntax-io.ioml"*)
(*#line 139.3 "syntax-io.ioml"*)
(*#line 139.3 "syntax-io.ioml"*)type 
(*#line 139.10 "syntax-io.ioml"*)
(*#line 139.10 "syntax-io.ioml"*)bytestring  = 
(*#line 139.23 "syntax-io.ioml"*)Word8Vector.vector  
(*#line 699.33 "syntax-io.sml"*)fun writeBytestring (p , x : bytestring  ) = 
(*#line 139.23 "syntax-io.ioml"*)
(*#line 139.23 "syntax-io.ioml"*)IOTypes.writeWord8Vector 
(*#line 702.33 "syntax-io.sml"*)(p , x )fun readBytestring p  = 
(*#line 139.23 "syntax-io.ioml"*)
(*#line 139.23 "syntax-io.ioml"*)IOTypes.readWord8Vector 
(*#line 705.33 "syntax-io.sml"*)p 
(*#line 140.3 "syntax-io.ioml"*)
(*#line 140.3 "syntax-io.ioml"*)
(*#line 140.3 "syntax-io.ioml"*)
(*#line 140.3 "syntax-io.ioml"*)type 
(*#line 140.10 "syntax-io.ioml"*)
(*#line 140.10 "syntax-io.ioml"*)crypto_sig  = 
(*#line 140.23 "syntax-io.ioml"*)bytestring  
(*#line 713.33 "syntax-io.sml"*)fun writeCrypto_sig (p , x : crypto_sig  ) = writeBytestring (p , x )fun readCrypto_sig p  = readBytestring p 
(*#line 141.3 "syntax-io.ioml"*)
(*#line 141.3 "syntax-io.ioml"*)
(*#line 141.3 "syntax-io.ioml"*)
(*#line 141.3 "syntax-io.ioml"*)type 
(*#line 141.10 "syntax-io.ioml"*)
(*#line 141.10 "syntax-io.ioml"*)crypto_address  = 
(*#line 141.27 "syntax-io.ioml"*)bytestring  
(*#line 721.33 "syntax-io.sml"*)fun writeCrypto_address (p , x : crypto_address  ) = writeBytestring (p , x )fun readCrypto_address p  = readBytestring p 
(*#line 142.3 "syntax-io.ioml"*)
(*#line 142.3 "syntax-io.ioml"*)
(*#line 142.3 "syntax-io.ioml"*)
(*#line 142.3 "syntax-io.ioml"*)type 
(*#line 142.10 "syntax-io.ioml"*)
(*#line 142.10 "syntax-io.ioml"*)crypto_principal  = 
(*#line 142.29 "syntax-io.ioml"*)bytestring  
(*#line 729.33 "syntax-io.sml"*)fun writeCrypto_principal (p , x : crypto_principal  ) = writeBytestring (p , x )fun readCrypto_principal p  = readBytestring p 
(*#line 144.3 "syntax-io.ioml"*)
(*#line 144.3 "syntax-io.ioml"*)
(*#line 144.3 "syntax-io.ioml"*)
(*#line 144.3 "syntax-io.ioml"*)type 
(*#line 144.10 "syntax-io.ioml"*)
(*#line 144.10 "syntax-io.ioml"*)signed_affirmation  = 
(*#line 145.8 "syntax-io.ioml"*){persistent : 
(*#line 145.21 "syntax-io.ioml"*)bool  , principal : 
(*#line 146.20 "syntax-io.ioml"*)crypto_principal  , crypto_sig : 
(*#line 148.21 "syntax-io.ioml"*)crypto_sig  , prop : 
(*#line 147.15 "syntax-io.ioml"*)prop  }
(*#line 741.33 "syntax-io.sml"*)fun writeSigned_affirmation (p , x : signed_affirmation  ) = ((fn x  => IOTypes.writeBool (p , x ))(# persistent x ); (fn x  => writeCrypto_principal (p , x ))(# principal x ); (fn x  => writeCrypto_sig (p , x ))(# crypto_sig x ); (fn x  => writeProp (p , x ))(# prop x ))fun readSigned_affirmation p  = case IOTypes.readBool p of NONE  => NONE  | SOME ypersistent  => (case readCrypto_principal p of NONE  => NONE  | SOME yprincipal  => (case readCrypto_sig p of NONE  => NONE  | SOME ycrypto_sig  => (case readProp p of NONE  => NONE  | SOME yprop  => (SOME {persistent = ypersistent , principal = yprincipal , crypto_sig = ycrypto_sig , prop = yprop }))))
(*#line 151.3 "syntax-io.ioml"*)
(*#line 151.3 "syntax-io.ioml"*)
(*#line 151.3 "syntax-io.ioml"*)
(*#line 151.14 "syntax-io.ioml"*)
(*#line 151.14 "syntax-io.ioml"*)datatype 
(*#line 151.14 "syntax-io.ioml"*)
(*#line 151.14 "syntax-io.ioml"*)proof  = 
(*#line 151.22 "syntax-io.ioml"*)MRule of 
(*#line 151.31 "syntax-io.ioml"*)const  | 
(*#line 152.20 "syntax-io.ioml"*)MVar of 
(*#line 152.28 "syntax-io.ioml"*)var  | 
(*#line 154.20 "syntax-io.ioml"*)MLet of 
(*#line 154.28 "syntax-io.ioml"*)(
(*#line 154.28 "syntax-io.ioml"*)proof   * 
(*#line 154.36 "syntax-io.ioml"*)var   * 
(*#line 154.42 "syntax-io.ioml"*)proof  )| 
(*#line 156.20 "syntax-io.ioml"*)MBang of 
(*#line 156.29 "syntax-io.ioml"*)proof  | 
(*#line 157.20 "syntax-io.ioml"*)MBangLet of 
(*#line 157.32 "syntax-io.ioml"*)(
(*#line 157.32 "syntax-io.ioml"*)proof   * 
(*#line 157.40 "syntax-io.ioml"*)var   * 
(*#line 157.46 "syntax-io.ioml"*)proof  )| 
(*#line 158.20 "syntax-io.ioml"*)MLam of 
(*#line 158.28 "syntax-io.ioml"*)(
(*#line 158.28 "syntax-io.ioml"*)var   * 
(*#line 158.34 "syntax-io.ioml"*)prop   * 
(*#line 158.41 "syntax-io.ioml"*)proof  )| 
(*#line 159.20 "syntax-io.ioml"*)MApp of 
(*#line 159.28 "syntax-io.ioml"*)(
(*#line 159.28 "syntax-io.ioml"*)proof   * 
(*#line 159.36 "syntax-io.ioml"*)proof  )| 
(*#line 160.20 "syntax-io.ioml"*)MTensor of 
(*#line 160.31 "syntax-io.ioml"*)(
(*#line 160.31 "syntax-io.ioml"*)proof   * 
(*#line 160.39 "syntax-io.ioml"*)proof  )| 
(*#line 161.20 "syntax-io.ioml"*)MTensorLet of 
(*#line 161.34 "syntax-io.ioml"*)(
(*#line 161.34 "syntax-io.ioml"*)proof   * 
(*#line 161.42 "syntax-io.ioml"*)var   * 
(*#line 161.48 "syntax-io.ioml"*)var   * 
(*#line 161.54 "syntax-io.ioml"*)proof  )| 
(*#line 162.20 "syntax-io.ioml"*)MWith of 
(*#line 162.29 "syntax-io.ioml"*)(
(*#line 162.29 "syntax-io.ioml"*)proof   * 
(*#line 162.37 "syntax-io.ioml"*)proof  )| 
(*#line 163.20 "syntax-io.ioml"*)MPi of 
(*#line 163.27 "syntax-io.ioml"*)(
(*#line 163.27 "syntax-io.ioml"*)idx   * 
(*#line 163.33 "syntax-io.ioml"*)proof  )| 
(*#line 164.20 "syntax-io.ioml"*)MInj of 
(*#line 164.28 "syntax-io.ioml"*)(
(*#line 164.28 "syntax-io.ioml"*)idx   * 
(*#line 164.34 "syntax-io.ioml"*)proof   * 
(*#line 164.42 "syntax-io.ioml"*)prop  )| 
(*#line 165.20 "syntax-io.ioml"*)MCase of 
(*#line 165.29 "syntax-io.ioml"*)(
(*#line 165.29 "syntax-io.ioml"*)proof   * 
(*#line 165.37 "syntax-io.ioml"*)var   * 
(*#line 165.43 "syntax-io.ioml"*)proof   * 
(*#line 165.51 "syntax-io.ioml"*)var   * 
(*#line 165.57 "syntax-io.ioml"*)proof  )| 
(*#line 166.20 "syntax-io.ioml"*)MOne | 
(*#line 168.20 "syntax-io.ioml"*)MAbort of 
(*#line 168.30 "syntax-io.ioml"*)(
(*#line 168.30 "syntax-io.ioml"*)proof   * 
(*#line 168.38 "syntax-io.ioml"*)prop  )| 
(*#line 170.20 "syntax-io.ioml"*)MForallLam of 
(*#line 170.34 "syntax-io.ioml"*)(
(*#line 170.34 "syntax-io.ioml"*)LF.binding   * 
(*#line 170.47 "syntax-io.ioml"*)LF.exp   * 
(*#line 170.56 "syntax-io.ioml"*)proof  )| 
(*#line 171.20 "syntax-io.ioml"*)MForallApp of 
(*#line 171.34 "syntax-io.ioml"*)(
(*#line 171.34 "syntax-io.ioml"*)proof   * 
(*#line 171.42 "syntax-io.ioml"*)LF.exp  )| 
(*#line 172.20 "syntax-io.ioml"*)MPack of 
(*#line 172.29 "syntax-io.ioml"*)(
(*#line 172.29 "syntax-io.ioml"*)LF.exp   * 
(*#line 172.38 "syntax-io.ioml"*)proof   * 
(*#line 172.46 "syntax-io.ioml"*)prop  )| 
(*#line 173.20 "syntax-io.ioml"*)MUnpack of 
(*#line 173.31 "syntax-io.ioml"*)(
(*#line 173.31 "syntax-io.ioml"*)proof   * 
(*#line 173.39 "syntax-io.ioml"*)LF.binding   * 
(*#line 173.52 "syntax-io.ioml"*)var   * 
(*#line 173.58 "syntax-io.ioml"*)proof  )| 
(*#line 176.20 "syntax-io.ioml"*)MSayReturn of 
(*#line 176.34 "syntax-io.ioml"*)(
(*#line 176.34 "syntax-io.ioml"*)principal   * 
(*#line 176.46 "syntax-io.ioml"*)proof  )| 
(*#line 177.20 "syntax-io.ioml"*)MSayBind of 
(*#line 177.32 "syntax-io.ioml"*)(
(*#line 177.32 "syntax-io.ioml"*)proof   * 
(*#line 177.40 "syntax-io.ioml"*)var   * 
(*#line 177.46 "syntax-io.ioml"*)proof  )| 
(*#line 180.20 "syntax-io.ioml"*)MIfReturn of 
(*#line 180.33 "syntax-io.ioml"*)(
(*#line 180.33 "syntax-io.ioml"*)condition   * 
(*#line 180.45 "syntax-io.ioml"*)proof  )| 
(*#line 181.20 "syntax-io.ioml"*)MIfBind of 
(*#line 181.31 "syntax-io.ioml"*)(
(*#line 181.31 "syntax-io.ioml"*)proof   * 
(*#line 181.39 "syntax-io.ioml"*)var   * 
(*#line 181.45 "syntax-io.ioml"*)proof  )| 
(*#line 182.20 "syntax-io.ioml"*)MIfWeaken of 
(*#line 182.33 "syntax-io.ioml"*)(
(*#line 182.33 "syntax-io.ioml"*)condition   * 
(*#line 182.45 "syntax-io.ioml"*)proof  )| 
(*#line 183.20 "syntax-io.ioml"*)MIfSay of 
(*#line 183.30 "syntax-io.ioml"*)proof  | 
(*#line 186.20 "syntax-io.ioml"*)MAffirmation of 
(*#line 186.36 "syntax-io.ioml"*)signed_affirmation  
(*#line 855.33 "syntax-io.sml"*)fun writeProof (p , x ) = case x of MRule x  => (IOTypes.writeInt (p , 0 ); writeConst (p , x )) | MVar x  => (IOTypes.writeInt (p , 1 ); writeVar (p , x )) | MLet x  => (IOTypes.writeInt (p , 2 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MBang x  => (IOTypes.writeInt (p , 3 ); writeProof (p , x )) | MBangLet x  => (IOTypes.writeInt (p , 4 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MLam x  => (IOTypes.writeInt (p , 5 ); ((fn x  => writeVar (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MApp x  => (IOTypes.writeInt (p , 6 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MTensor x  => (IOTypes.writeInt (p , 7 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MTensorLet x  => (IOTypes.writeInt (p , 8 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeVar (p , x ))(# 3 x ); (fn x  => writeProof (p , x ))(# 4 x ))) | MWith x  => (IOTypes.writeInt (p , 9 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MPi x  => (IOTypes.writeInt (p , 10 ); ((fn x  => writeIdx (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MInj x  => (IOTypes.writeInt (p , 11 ); ((fn x  => writeIdx (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ); (fn x  => writeProp (p , x ))(# 3 x ))) | MCase x  => (IOTypes.writeInt (p , 12 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ); (fn x  => writeVar (p , x ))(# 4 x ); (fn x  => writeProof (p , x ))(# 5 x ))) | MOne  => IOTypes.writeInt (p , 13 ) | MAbort x  => (IOTypes.writeInt (p , 14 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | MForallLam x  => (IOTypes.writeInt (p , 15 ); (
(*#line 170.34 "syntax-io.ioml"*)
(*#line 170.34 "syntax-io.ioml"*)(
(*#line 170.34 "syntax-io.ioml"*)fn 
(*#line 170.34 "syntax-io.ioml"*)
(*#line 860.33 "syntax-io.sml"*)x  => 
(*#line 170.34 "syntax-io.ioml"*)
(*#line 170.34 "syntax-io.ioml"*)LF.writeBinding 
(*#line 863.33 "syntax-io.sml"*)(p , x ))(# 1 x ); 
(*#line 170.47 "syntax-io.ioml"*)
(*#line 170.47 "syntax-io.ioml"*)(
(*#line 170.47 "syntax-io.ioml"*)fn 
(*#line 170.47 "syntax-io.ioml"*)
(*#line 868.33 "syntax-io.sml"*)x  => 
(*#line 170.47 "syntax-io.ioml"*)
(*#line 170.47 "syntax-io.ioml"*)LF.writeExp 
(*#line 871.33 "syntax-io.sml"*)(p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MForallApp x  => (IOTypes.writeInt (p , 16 ); ((fn x  => writeProof (p , x ))(# 1 x ); 
(*#line 171.42 "syntax-io.ioml"*)
(*#line 171.42 "syntax-io.ioml"*)(
(*#line 171.42 "syntax-io.ioml"*)fn 
(*#line 171.42 "syntax-io.ioml"*)
(*#line 876.33 "syntax-io.sml"*)x  => 
(*#line 171.42 "syntax-io.ioml"*)
(*#line 171.42 "syntax-io.ioml"*)LF.writeExp 
(*#line 879.33 "syntax-io.sml"*)(p , x ))(# 2 x ))) | MPack x  => (IOTypes.writeInt (p , 17 ); (
(*#line 172.29 "syntax-io.ioml"*)
(*#line 172.29 "syntax-io.ioml"*)(
(*#line 172.29 "syntax-io.ioml"*)fn 
(*#line 172.29 "syntax-io.ioml"*)
(*#line 884.33 "syntax-io.sml"*)x  => 
(*#line 172.29 "syntax-io.ioml"*)
(*#line 172.29 "syntax-io.ioml"*)LF.writeExp 
(*#line 887.33 "syntax-io.sml"*)(p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ); (fn x  => writeProp (p , x ))(# 3 x ))) | MUnpack x  => (IOTypes.writeInt (p , 18 ); ((fn x  => writeProof (p , x ))(# 1 x ); 
(*#line 173.39 "syntax-io.ioml"*)
(*#line 173.39 "syntax-io.ioml"*)(
(*#line 173.39 "syntax-io.ioml"*)fn 
(*#line 173.39 "syntax-io.ioml"*)
(*#line 892.33 "syntax-io.sml"*)x  => 
(*#line 173.39 "syntax-io.ioml"*)
(*#line 173.39 "syntax-io.ioml"*)LF.writeBinding 
(*#line 895.33 "syntax-io.sml"*)(p , x ))(# 2 x ); (fn x  => writeVar (p , x ))(# 3 x ); (fn x  => writeProof (p , x ))(# 4 x ))) | MSayReturn x  => (IOTypes.writeInt (p , 19 ); ((fn x  => writePrincipal (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MSayBind x  => (IOTypes.writeInt (p , 20 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MIfReturn x  => (IOTypes.writeInt (p , 21 ); ((fn x  => writeCondition (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MIfBind x  => (IOTypes.writeInt (p , 22 ); ((fn x  => writeProof (p , x ))(# 1 x ); (fn x  => writeVar (p , x ))(# 2 x ); (fn x  => writeProof (p , x ))(# 3 x ))) | MIfWeaken x  => (IOTypes.writeInt (p , 23 ); ((fn x  => writeCondition (p , x ))(# 1 x ); (fn x  => writeProof (p , x ))(# 2 x ))) | MIfSay x  => (IOTypes.writeInt (p , 24 ); writeProof (p , x )) | MAffirmation x  => (IOTypes.writeInt (p , 25 ); writeSigned_affirmation (p , x ))fun readProof p  = case IOTypes.readInt p of SOME 0  => (case readConst p of NONE  => NONE  | SOME x  => SOME (MRule x )) | SOME 1  => (case readVar p of NONE  => NONE  | SOME x  => SOME (MVar x )) | SOME 2  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MLet x )) | SOME 3  => (case readProof p of NONE  => NONE  | SOME x  => SOME (MBang x )) | SOME 4  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MBangLet x )) | SOME 5  => (case case readVar p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MLam x )) | SOME 6  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MApp x )) | SOME 7  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MTensor x )) | SOME 8  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readVar p of NONE  => NONE  | SOME y2  => (case readProof p of NONE  => NONE  | SOME y3  => (SOME (y0 , y1 , y2 , y3 )))))of NONE  => NONE  | SOME x  => SOME (MTensorLet x )) | SOME 9  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MWith x )) | SOME 10  => (case case readIdx p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MPi x )) | SOME 11  => (case case readIdx p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (case readProp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MInj x )) | SOME 12  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (case readVar p of NONE  => NONE  | SOME y3  => (case readProof p of NONE  => NONE  | SOME y4  => (SOME (y0 , y1 , y2 , y3 , y4 ))))))of NONE  => NONE  | SOME x  => SOME (MCase x )) | SOME 13  => SOME MOne  | SOME 14  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MAbort x )) | SOME 15  => (case case 
(*#line 170.34 "syntax-io.ioml"*)
(*#line 170.34 "syntax-io.ioml"*)LF.readBinding 
(*#line 898.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case 
(*#line 170.47 "syntax-io.ioml"*)
(*#line 170.47 "syntax-io.ioml"*)LF.readExp 
(*#line 901.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MForallLam x )) | SOME 16  => (case case readProof p of NONE  => NONE  | SOME y0  => (case 
(*#line 171.42 "syntax-io.ioml"*)
(*#line 171.42 "syntax-io.ioml"*)LF.readExp 
(*#line 904.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MForallApp x )) | SOME 17  => (case case 
(*#line 172.29 "syntax-io.ioml"*)
(*#line 172.29 "syntax-io.ioml"*)LF.readExp 
(*#line 907.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (case readProp p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MPack x )) | SOME 18  => (case case readProof p of NONE  => NONE  | SOME y0  => (case 
(*#line 173.39 "syntax-io.ioml"*)
(*#line 173.39 "syntax-io.ioml"*)LF.readBinding 
(*#line 910.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y1  => (case readVar p of NONE  => NONE  | SOME y2  => (case readProof p of NONE  => NONE  | SOME y3  => (SOME (y0 , y1 , y2 , y3 )))))of NONE  => NONE  | SOME x  => SOME (MUnpack x )) | SOME 19  => (case case readPrincipal p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MSayReturn x )) | SOME 20  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MSayBind x )) | SOME 21  => (case case readCondition p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MIfReturn x )) | SOME 22  => (case case readProof p of NONE  => NONE  | SOME y0  => (case readVar p of NONE  => NONE  | SOME y1  => (case readProof p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))of NONE  => NONE  | SOME x  => SOME (MIfBind x )) | SOME 23  => (case case readCondition p of NONE  => NONE  | SOME y0  => (case readProof p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (MIfWeaken x )) | SOME 24  => (case readProof p of NONE  => NONE  | SOME x  => SOME (MIfSay x )) | SOME 25  => (case readSigned_affirmation p of NONE  => NONE  | SOME x  => SOME (MAffirmation x )) | _  => NONE 
(*#line 188.3 "syntax-io.ioml"*)
(*#line 188.3 "syntax-io.ioml"*)
(*#line 188.3 "syntax-io.ioml"*)
(*#line 188.14 "syntax-io.ioml"*)
(*#line 188.14 "syntax-io.ioml"*)datatype 
(*#line 188.14 "syntax-io.ioml"*)
(*#line 188.14 "syntax-io.ioml"*)basis_entry  = 
(*#line 188.28 "syntax-io.ioml"*)SRule of 
(*#line 188.37 "syntax-io.ioml"*)(
(*#line 188.37 "syntax-io.ioml"*)Const.id   * 
(*#line 188.48 "syntax-io.ioml"*)prop  )| 
(*#line 189.26 "syntax-io.ioml"*)SConst of 
(*#line 189.36 "syntax-io.ioml"*)LFSyntax.basis_entry  
(*#line 924.33 "syntax-io.sml"*)fun writeBasis_entry (p , x ) = case x of SRule x  => (IOTypes.writeInt (p , 0 ); (
(*#line 188.37 "syntax-io.ioml"*)
(*#line 188.37 "syntax-io.ioml"*)(
(*#line 188.37 "syntax-io.ioml"*)fn 
(*#line 188.37 "syntax-io.ioml"*)
(*#line 929.33 "syntax-io.sml"*)x  => 
(*#line 188.37 "syntax-io.ioml"*)
(*#line 188.37 "syntax-io.ioml"*)Const.writeId 
(*#line 932.33 "syntax-io.sml"*)(p , x ))(# 1 x ); (fn x  => writeProp (p , x ))(# 2 x ))) | SConst x  => 
(*#line 189.36 "syntax-io.ioml"*)(
(*#line 934.33 "syntax-io.sml"*)IOTypes.writeInt (p , 1 ); 
(*#line 189.36 "syntax-io.ioml"*)
(*#line 189.36 "syntax-io.ioml"*)LFSyntax.writeBasis_entry 
(*#line 937.33 "syntax-io.sml"*)(p , x ))fun readBasis_entry p  = case IOTypes.readInt p of SOME 0  => (case case 
(*#line 188.37 "syntax-io.ioml"*)
(*#line 188.37 "syntax-io.ioml"*)Const.readId 
(*#line 940.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case readProp p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (SRule x )) | SOME 1  => (case 
(*#line 189.36 "syntax-io.ioml"*)
(*#line 189.36 "syntax-io.ioml"*)LFSyntax.readBasis_entry 
(*#line 943.33 "syntax-io.sml"*)p of NONE  => NONE  | SOME x  => SOME (SConst x )) | _  => NONE 
(*#line 190.3 "syntax-io.ioml"*)
(*#line 190.3 "syntax-io.ioml"*)
(*#line 190.3 "syntax-io.ioml"*)
(*#line 190.3 "syntax-io.ioml"*)type 
(*#line 190.10 "syntax-io.ioml"*)
(*#line 190.10 "syntax-io.ioml"*)basis  = 
(*#line 190.18 "syntax-io.ioml"*)
(*#line 190.18 "syntax-io.ioml"*)basis_entry   list  
(*#line 952.33 "syntax-io.sml"*)fun writeBasis (p , x : basis  ) = IOTypes.writeList (writeBasis_entry )(p , x )fun readBasis p  = IOTypes.readList (readBasis_entry )p 
(*#line 192.1 "syntax-io.ioml"*)end 
(*#line 194.1 "syntax-io.ioml"*)
(*#line 194.1 "syntax-io.ioml"*)structure TypeCoinTxn = 
(*#line 195.1 "syntax-io.ioml"*)struct 
(*#line 196.3 "syntax-io.ioml"*)
(*#line 196.3 "syntax-io.ioml"*)structure TxnDict = 
(*#line 196.23 "syntax-io.ioml"*)StringSplayDict 
(*#line 198.3 "syntax-io.ioml"*)
(*#line 198.3 "syntax-io.ioml"*)
(*#line 198.3 "syntax-io.ioml"*)
(*#line 198.3 "syntax-io.ioml"*)type 
(*#line 198.10 "syntax-io.ioml"*)
(*#line 198.10 "syntax-io.ioml"*)txnid  = 
(*#line 198.18 "syntax-io.ioml"*)Const.namespace  
(*#line 967.33 "syntax-io.sml"*)fun writeTxnid (p , x : txnid  ) = 
(*#line 198.18 "syntax-io.ioml"*)
(*#line 198.18 "syntax-io.ioml"*)Const.writeNamespace 
(*#line 970.33 "syntax-io.sml"*)(p , x )fun readTxnid p  = 
(*#line 198.18 "syntax-io.ioml"*)
(*#line 198.18 "syntax-io.ioml"*)Const.readNamespace 
(*#line 973.33 "syntax-io.sml"*)p 
(*#line 200.3 "syntax-io.ioml"*)
(*#line 200.3 "syntax-io.ioml"*)
(*#line 200.3 "syntax-io.ioml"*)
(*#line 200.3 "syntax-io.ioml"*)type 
(*#line 200.10 "syntax-io.ioml"*)
(*#line 200.10 "syntax-io.ioml"*)crypto_sig  = 
(*#line 200.23 "syntax-io.ioml"*)Logic.crypto_sig  
(*#line 981.33 "syntax-io.sml"*)fun writeCrypto_sig (p , x : crypto_sig  ) = 
(*#line 200.23 "syntax-io.ioml"*)
(*#line 200.23 "syntax-io.ioml"*)Logic.writeCrypto_sig 
(*#line 984.33 "syntax-io.sml"*)(p , x )fun readCrypto_sig p  = 
(*#line 200.23 "syntax-io.ioml"*)
(*#line 200.23 "syntax-io.ioml"*)Logic.readCrypto_sig 
(*#line 987.33 "syntax-io.sml"*)p 
(*#line 201.3 "syntax-io.ioml"*)
(*#line 201.3 "syntax-io.ioml"*)
(*#line 201.3 "syntax-io.ioml"*)
(*#line 201.3 "syntax-io.ioml"*)type 
(*#line 201.10 "syntax-io.ioml"*)
(*#line 201.10 "syntax-io.ioml"*)crypto_address  = 
(*#line 201.27 "syntax-io.ioml"*)Logic.crypto_address  
(*#line 995.33 "syntax-io.sml"*)fun writeCrypto_address (p , x : crypto_address  ) = 
(*#line 201.27 "syntax-io.ioml"*)
(*#line 201.27 "syntax-io.ioml"*)Logic.writeCrypto_address 
(*#line 998.33 "syntax-io.sml"*)(p , x )fun readCrypto_address p  = 
(*#line 201.27 "syntax-io.ioml"*)
(*#line 201.27 "syntax-io.ioml"*)Logic.readCrypto_address 
(*#line 1001.34 "syntax-io.sml"*)p 
(*#line 202.3 "syntax-io.ioml"*)
(*#line 202.3 "syntax-io.ioml"*)
(*#line 202.3 "syntax-io.ioml"*)
(*#line 202.3 "syntax-io.ioml"*)type 
(*#line 202.10 "syntax-io.ioml"*)
(*#line 202.10 "syntax-io.ioml"*)crypto_principal  = 
(*#line 202.29 "syntax-io.ioml"*)Logic.crypto_principal  
(*#line 1009.34 "syntax-io.sml"*)fun writeCrypto_principal (p , x : crypto_principal  ) = 
(*#line 202.29 "syntax-io.ioml"*)
(*#line 202.29 "syntax-io.ioml"*)Logic.writeCrypto_principal 
(*#line 1012.34 "syntax-io.sml"*)(p , x )fun readCrypto_principal p  = 
(*#line 202.29 "syntax-io.ioml"*)
(*#line 202.29 "syntax-io.ioml"*)Logic.readCrypto_principal 
(*#line 1015.34 "syntax-io.sml"*)p 
(*#line 204.3 "syntax-io.ioml"*)
(*#line 204.3 "syntax-io.ioml"*)
(*#line 204.3 "syntax-io.ioml"*)
(*#line 204.3 "syntax-io.ioml"*)type 
(*#line 204.10 "syntax-io.ioml"*)
(*#line 204.10 "syntax-io.ioml"*)amount  = 
(*#line 204.19 "syntax-io.ioml"*)IntInf.int  
(*#line 1023.34 "syntax-io.sml"*)fun writeAmount (p , x : amount  ) = 
(*#line 204.19 "syntax-io.ioml"*)
(*#line 204.19 "syntax-io.ioml"*)IOIntInf.writeInt 
(*#line 1026.34 "syntax-io.sml"*)(p , x )fun readAmount p  = 
(*#line 204.19 "syntax-io.ioml"*)
(*#line 204.19 "syntax-io.ioml"*)IOIntInf.readInt 
(*#line 1029.34 "syntax-io.sml"*)p 
(*#line 206.3 "syntax-io.ioml"*)
(*#line 206.3 "syntax-io.ioml"*)
(*#line 206.3 "syntax-io.ioml"*)
(*#line 206.14 "syntax-io.ioml"*)
(*#line 206.14 "syntax-io.ioml"*)datatype 
(*#line 206.14 "syntax-io.ioml"*)
(*#line 206.14 "syntax-io.ioml"*)input  = 
(*#line 206.22 "syntax-io.ioml"*)Input of 
(*#line 207.12 "syntax-io.ioml"*){prop : 
(*#line 208.19 "syntax-io.ioml"*)Logic.prop  , source : 
(*#line 207.21 "syntax-io.ioml"*)(
(*#line 207.21 "syntax-io.ioml"*)txnid   * 
(*#line 207.29 "syntax-io.ioml"*)int  )}
(*#line 1043.34 "syntax-io.sml"*)fun writeInput (p , x ) = case x of Input x  => (IOTypes.writeInt (p , 0 ); (
(*#line 208.19 "syntax-io.ioml"*)
(*#line 208.19 "syntax-io.ioml"*)(
(*#line 208.19 "syntax-io.ioml"*)fn 
(*#line 208.19 "syntax-io.ioml"*)
(*#line 1048.34 "syntax-io.sml"*)x  => 
(*#line 208.19 "syntax-io.ioml"*)
(*#line 208.19 "syntax-io.ioml"*)Logic.writeProp 
(*#line 1051.34 "syntax-io.sml"*)(p , x ))(# prop x ); (fn x  => ((fn x  => writeTxnid (p , x ))(# 1 x ); (fn x  => IOTypes.writeInt (p , x ))(# 2 x )))(# source x )))fun readInput p  = case IOTypes.readInt p of SOME 0  => (case case 
(*#line 208.19 "syntax-io.ioml"*)
(*#line 208.19 "syntax-io.ioml"*)Logic.readProp 
(*#line 1054.34 "syntax-io.sml"*)p of NONE  => NONE  | SOME yprop  => (case case readTxnid p of NONE  => NONE  | SOME y0  => (case IOTypes.readInt p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME ysource  => (SOME {prop = yprop , source = ysource }))of NONE  => NONE  | SOME x  => SOME (Input x )) | _  => NONE 
(*#line 210.3 "syntax-io.ioml"*)
(*#line 210.3 "syntax-io.ioml"*)
(*#line 210.3 "syntax-io.ioml"*)
(*#line 210.3 "syntax-io.ioml"*)type 
(*#line 210.10 "syntax-io.ioml"*)
(*#line 210.10 "syntax-io.ioml"*)inputs  = 
(*#line 210.19 "syntax-io.ioml"*)
(*#line 210.19 "syntax-io.ioml"*)input   list  
(*#line 1063.34 "syntax-io.sml"*)fun writeInputs (p , x : inputs  ) = IOTypes.writeList (writeInput )(p , x )fun readInputs p  = IOTypes.readList (readInput )p 
(*#line 212.3 "syntax-io.ioml"*)
(*#line 212.3 "syntax-io.ioml"*)
(*#line 212.3 "syntax-io.ioml"*)
(*#line 212.14 "syntax-io.ioml"*)
(*#line 212.14 "syntax-io.ioml"*)datatype 
(*#line 212.14 "syntax-io.ioml"*)
(*#line 212.14 "syntax-io.ioml"*)output  = 
(*#line 212.23 "syntax-io.ioml"*)Output of 
(*#line 213.12 "syntax-io.ioml"*){dest : 
(*#line 213.19 "syntax-io.ioml"*)crypto_address  , amount : 
(*#line 216.21 "syntax-io.ioml"*)
(*#line 216.21 "syntax-io.ioml"*)amount   option  , needs_receipt : 
(*#line 215.28 "syntax-io.ioml"*)bool  , prop : 
(*#line 214.19 "syntax-io.ioml"*)Logic.prop  }
(*#line 1078.34 "syntax-io.sml"*)fun writeOutput (p , x ) = case x of Output x  => (IOTypes.writeInt (p , 0 ); ((fn x  => writeCrypto_address (p , x ))(# dest x ); (fn x  => IOTypes.writeOption (writeAmount )(p , x ))(# amount x ); (fn x  => IOTypes.writeBool (p , x ))(# needs_receipt x ); 
(*#line 214.19 "syntax-io.ioml"*)
(*#line 214.19 "syntax-io.ioml"*)(
(*#line 214.19 "syntax-io.ioml"*)fn 
(*#line 214.19 "syntax-io.ioml"*)
(*#line 1083.34 "syntax-io.sml"*)x  => 
(*#line 214.19 "syntax-io.ioml"*)
(*#line 214.19 "syntax-io.ioml"*)Logic.writeProp 
(*#line 1086.34 "syntax-io.sml"*)(p , x ))(# prop x )))fun readOutput p  = case IOTypes.readInt p of SOME 0  => (case case readCrypto_address p of NONE  => NONE  | SOME ydest  => (case IOTypes.readOption (readAmount )p of NONE  => NONE  | SOME yamount  => (case IOTypes.readBool p of NONE  => NONE  | SOME yneeds_receipt  => (case 
(*#line 214.19 "syntax-io.ioml"*)
(*#line 214.19 "syntax-io.ioml"*)Logic.readProp 
(*#line 1089.34 "syntax-io.sml"*)p of NONE  => NONE  | SOME yprop  => (SOME {dest = ydest , amount = yamount , needs_receipt = yneeds_receipt , prop = yprop }))))of NONE  => NONE  | SOME x  => SOME (Output x )) | _  => NONE 
(*#line 218.3 "syntax-io.ioml"*)
(*#line 218.3 "syntax-io.ioml"*)
(*#line 218.3 "syntax-io.ioml"*)
(*#line 218.3 "syntax-io.ioml"*)type 
(*#line 218.10 "syntax-io.ioml"*)
(*#line 218.10 "syntax-io.ioml"*)outputs  = 
(*#line 218.20 "syntax-io.ioml"*)
(*#line 218.20 "syntax-io.ioml"*)output   list  
(*#line 1098.34 "syntax-io.sml"*)fun writeOutputs (p , x : outputs  ) = IOTypes.writeList (writeOutput )(p , x )fun readOutputs p  = IOTypes.readList (readOutput )p 
(*#line 220.3 "syntax-io.ioml"*)
(*#line 220.3 "syntax-io.ioml"*)
(*#line 220.3 "syntax-io.ioml"*)
(*#line 220.3 "syntax-io.ioml"*)type 
(*#line 220.10 "syntax-io.ioml"*)
(*#line 220.10 "syntax-io.ioml"*)basis  = 
(*#line 220.18 "syntax-io.ioml"*)
(*#line 220.18 "syntax-io.ioml"*)Logic.basis_entry   list  
(*#line 1107.34 "syntax-io.sml"*)fun writeBasis (p , x : basis  ) = 
(*#line 220.18 "syntax-io.ioml"*)
(*#line 220.18 "syntax-io.ioml"*)
(*#line 1110.34 "syntax-io.sml"*)IOTypes.writeList 
(*#line 220.18 "syntax-io.ioml"*)(
(*#line 220.18 "syntax-io.ioml"*)Logic.writeBasis_entry )
(*#line 1113.34 "syntax-io.sml"*)(p , x )fun readBasis p  = 
(*#line 220.18 "syntax-io.ioml"*)
(*#line 220.18 "syntax-io.ioml"*)
(*#line 1116.34 "syntax-io.sml"*)IOTypes.readList 
(*#line 220.18 "syntax-io.ioml"*)(
(*#line 220.18 "syntax-io.ioml"*)Logic.readBasis_entry )
(*#line 1119.34 "syntax-io.sml"*)p 
(*#line 223.3 "syntax-io.ioml"*)
(*#line 223.3 "syntax-io.ioml"*)
(*#line 223.3 "syntax-io.ioml"*)
(*#line 223.3 "syntax-io.ioml"*)type 
(*#line 223.10 "syntax-io.ioml"*)
(*#line 223.10 "syntax-io.ioml"*)linear_grant  = 
(*#line 223.25 "syntax-io.ioml"*)
(*#line 223.25 "syntax-io.ioml"*)Logic.prop   list  
(*#line 1128.34 "syntax-io.sml"*)fun writeLinear_grant (p , x : linear_grant  ) = 
(*#line 223.25 "syntax-io.ioml"*)
(*#line 223.25 "syntax-io.ioml"*)
(*#line 1131.34 "syntax-io.sml"*)IOTypes.writeList 
(*#line 223.25 "syntax-io.ioml"*)(
(*#line 223.25 "syntax-io.ioml"*)Logic.writeProp )
(*#line 1134.34 "syntax-io.sml"*)(p , x )fun readLinear_grant p  = 
(*#line 223.25 "syntax-io.ioml"*)
(*#line 223.25 "syntax-io.ioml"*)
(*#line 1137.34 "syntax-io.sml"*)IOTypes.readList 
(*#line 223.25 "syntax-io.ioml"*)(
(*#line 223.25 "syntax-io.ioml"*)Logic.readProp )
(*#line 1140.34 "syntax-io.sml"*)p 
(*#line 226.3 "syntax-io.ioml"*)
(*#line 226.3 "syntax-io.ioml"*)
(*#line 226.3 "syntax-io.ioml"*)
(*#line 226.14 "syntax-io.ioml"*)
(*#line 226.14 "syntax-io.ioml"*)datatype 
(*#line 226.14 "syntax-io.ioml"*)
(*#line 226.14 "syntax-io.ioml"*)txn_body  = 
(*#line 226.25 "syntax-io.ioml"*)TxnBody of 
(*#line 227.12 "syntax-io.ioml"*){name : 
(*#line 227.19 "syntax-io.ioml"*)string  , metadata : 
(*#line 228.23 "syntax-io.ioml"*)
(*#line 228.23 "syntax-io.ioml"*)string   list  , inputs : 
(*#line 229.21 "syntax-io.ioml"*)inputs  , basis : 
(*#line 230.20 "syntax-io.ioml"*)basis  , linear_grant : 
(*#line 231.27 "syntax-io.ioml"*)linear_grant  , outputs : 
(*#line 232.22 "syntax-io.ioml"*)outputs  , proof_term : 
(*#line 233.25 "syntax-io.ioml"*)Logic.proof  }
(*#line 1158.34 "syntax-io.sml"*)fun writeTxn_body (p , x ) = case x of TxnBody x  => (IOTypes.writeInt (p , 0 ); ((fn x  => IOTypes.writeString (p , x ))(# name x ); (fn x  => IOTypes.writeList (IOTypes.writeString )(p , x ))(# metadata x ); (fn x  => writeInputs (p , x ))(# inputs x ); (fn x  => writeBasis (p , x ))(# basis x ); (fn x  => writeLinear_grant (p , x ))(# linear_grant x ); (fn x  => writeOutputs (p , x ))(# outputs x ); 
(*#line 233.25 "syntax-io.ioml"*)
(*#line 233.25 "syntax-io.ioml"*)(
(*#line 233.25 "syntax-io.ioml"*)fn 
(*#line 233.25 "syntax-io.ioml"*)
(*#line 1163.34 "syntax-io.sml"*)x  => 
(*#line 233.25 "syntax-io.ioml"*)
(*#line 233.25 "syntax-io.ioml"*)Logic.writeProof 
(*#line 1166.34 "syntax-io.sml"*)(p , x ))(# proof_term x )))fun readTxn_body p  = case IOTypes.readInt p of SOME 0  => (case case IOTypes.readString p of NONE  => NONE  | SOME yname  => (case IOTypes.readList (IOTypes.readString )p of NONE  => NONE  | SOME ymetadata  => (case readInputs p of NONE  => NONE  | SOME yinputs  => (case readBasis p of NONE  => NONE  | SOME ybasis  => (case readLinear_grant p of NONE  => NONE  | SOME ylinear_grant  => (case readOutputs p of NONE  => NONE  | SOME youtputs  => (case 
(*#line 233.25 "syntax-io.ioml"*)
(*#line 233.25 "syntax-io.ioml"*)Logic.readProof 
(*#line 1169.34 "syntax-io.sml"*)p of NONE  => NONE  | SOME yproof_term  => (SOME {name = yname , metadata = ymetadata , inputs = yinputs , basis = ybasis , linear_grant = ylinear_grant , outputs = youtputs , proof_term = yproof_term })))))))of NONE  => NONE  | SOME x  => SOME (TxnBody x )) | _  => NONE 
(*#line 234.3 "syntax-io.ioml"*)
(*#line 234.3 "syntax-io.ioml"*)
(*#line 234.3 "syntax-io.ioml"*)
(*#line 234.3 "syntax-io.ioml"*)type 
(*#line 234.10 "syntax-io.ioml"*)
(*#line 234.10 "syntax-io.ioml"*)txn_bodies  = 
(*#line 234.23 "syntax-io.ioml"*)
(*#line 234.23 "syntax-io.ioml"*)txn_body   list  
(*#line 1178.34 "syntax-io.sml"*)fun writeTxn_bodies (p , x : txn_bodies  ) = IOTypes.writeList (writeTxn_body )(p , x )fun readTxn_bodies p  = IOTypes.readList (readTxn_body )p 
(*#line 235.3 "syntax-io.ioml"*)
(*#line 235.3 "syntax-io.ioml"*)
(*#line 235.3 "syntax-io.ioml"*)
(*#line 235.3 "syntax-io.ioml"*)type 
(*#line 235.10 "syntax-io.ioml"*)
(*#line 235.10 "syntax-io.ioml"*)txn  = 
(*#line 235.16 "syntax-io.ioml"*)(
(*#line 235.16 "syntax-io.ioml"*)
(*#line 235.16 "syntax-io.ioml"*)int   option   * 
(*#line 235.29 "syntax-io.ioml"*)txnid   * 
(*#line 235.37 "syntax-io.ioml"*)txn_bodies  )
(*#line 1190.34 "syntax-io.sml"*)fun writeTxn (p , x : txn  ) = ((fn x  => IOTypes.writeOption (IOTypes.writeInt )(p , x ))(# 1 x ); (fn x  => writeTxnid (p , x ))(# 2 x ); (fn x  => writeTxn_bodies (p , x ))(# 3 x ))fun readTxn p  = case IOTypes.readOption (IOTypes.readInt )p of NONE  => NONE  | SOME y0  => (case readTxnid p of NONE  => NONE  | SOME y1  => (case readTxn_bodies p of NONE  => NONE  | SOME y2  => (SOME (y0 , y1 , y2 ))))
(*#line 237.3 "syntax-io.ioml"*)
(*#line 237.3 "syntax-io.ioml"*)
(*#line 237.3 "syntax-io.ioml"*)
(*#line 237.3 "syntax-io.ioml"*)type 
(*#line 237.10 "syntax-io.ioml"*)
(*#line 237.10 "syntax-io.ioml"*)chain  = 
(*#line 237.18 "syntax-io.ioml"*)
(*#line 237.18 "syntax-io.ioml"*)txn   list  
(*#line 1199.34 "syntax-io.sml"*)fun writeChain (p , x : chain  ) = IOTypes.writeList (writeTxn )(p , x )fun readChain p  = IOTypes.readList (readTxn )p 
(*#line 240.3 "syntax-io.ioml"*)
(*#line 240.3 "syntax-io.ioml"*)
(*#line 240.3 "syntax-io.ioml"*)fun 
(*#line 240.7 "syntax-io.ioml"*)fromHexId 
(*#line 240.17 "syntax-io.ioml"*)id  = 
(*#line 240.22 "syntax-io.ioml"*)
(*#line 240.22 "syntax-io.ioml"*)Bytestring.rev 
(*#line 240.37 "syntax-io.ioml"*)(
(*#line 240.38 "syntax-io.ioml"*)
(*#line 240.38 "syntax-io.ioml"*)valOf 
(*#line 240.44 "syntax-io.ioml"*)(
(*#line 240.45 "syntax-io.ioml"*)
(*#line 240.45 "syntax-io.ioml"*)Bytestring.fromStringHex 
(*#line 240.70 "syntax-io.ioml"*)id ))
(*#line 241.3 "syntax-io.ioml"*)
(*#line 241.3 "syntax-io.ioml"*)
(*#line 241.3 "syntax-io.ioml"*)fun 
(*#line 241.7 "syntax-io.ioml"*)toHexId 
(*#line 241.15 "syntax-io.ioml"*)id  = 
(*#line 241.20 "syntax-io.ioml"*)
(*#line 241.20 "syntax-io.ioml"*)Bytestring.toStringHex 
(*#line 241.43 "syntax-io.ioml"*)(
(*#line 241.44 "syntax-io.ioml"*)
(*#line 241.44 "syntax-io.ioml"*)Bytestring.rev 
(*#line 241.59 "syntax-io.ioml"*)id )
(*#line 243.1 "syntax-io.ioml"*)end 
(*#line 248.1 "syntax-io.ioml"*)
(*#line 248.1 "syntax-io.ioml"*)structure BatchData = 
(*#line 249.1 "syntax-io.ioml"*)struct 
(*#line 250.3 "syntax-io.ioml"*)
(*#line 250.3 "syntax-io.ioml"*)
(*#line 250.3 "syntax-io.ioml"*)
(*#line 250.14 "syntax-io.ioml"*)
(*#line 250.14 "syntax-io.ioml"*)datatype 
(*#line 250.14 "syntax-io.ioml"*)
(*#line 250.14 "syntax-io.ioml"*)res_location  = 
(*#line 251.12 "syntax-io.ioml"*)RealTxout of 
(*#line 251.25 "syntax-io.ioml"*)(
(*#line 251.25 "syntax-io.ioml"*)TypeCoinTxn.txnid   * 
(*#line 251.45 "syntax-io.ioml"*)int  )| 
(*#line 252.12 "syntax-io.ioml"*)BatchTxout of 
(*#line 252.26 "syntax-io.ioml"*)(
(*#line 252.26 "syntax-io.ioml"*)Int32.int   * 
(*#line 252.38 "syntax-io.ioml"*)int  )
(*#line 1244.34 "syntax-io.sml"*)fun writeRes_location (p , x ) = case x of RealTxout x  => (IOTypes.writeInt (p , 0 ); (
(*#line 251.25 "syntax-io.ioml"*)
(*#line 251.25 "syntax-io.ioml"*)(
(*#line 251.25 "syntax-io.ioml"*)fn 
(*#line 251.25 "syntax-io.ioml"*)
(*#line 1249.34 "syntax-io.sml"*)x  => 
(*#line 251.25 "syntax-io.ioml"*)
(*#line 251.25 "syntax-io.ioml"*)TypeCoinTxn.writeTxnid 
(*#line 1252.34 "syntax-io.sml"*)(p , x ))(# 1 x ); (fn x  => IOTypes.writeInt (p , x ))(# 2 x ))) | BatchTxout x  => (IOTypes.writeInt (p , 1 ); (
(*#line 252.26 "syntax-io.ioml"*)
(*#line 252.26 "syntax-io.ioml"*)(
(*#line 252.26 "syntax-io.ioml"*)fn 
(*#line 252.26 "syntax-io.ioml"*)
(*#line 1257.34 "syntax-io.sml"*)x  => 
(*#line 252.26 "syntax-io.ioml"*)
(*#line 252.26 "syntax-io.ioml"*)IOInt32.writeInt 
(*#line 1260.34 "syntax-io.sml"*)(p , x ))(# 1 x ); (fn x  => IOTypes.writeInt (p , x ))(# 2 x )))fun readRes_location p  = case IOTypes.readInt p of SOME 0  => (case case 
(*#line 251.25 "syntax-io.ioml"*)
(*#line 251.25 "syntax-io.ioml"*)TypeCoinTxn.readTxnid 
(*#line 1263.34 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case IOTypes.readInt p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (RealTxout x )) | SOME 1  => (case case 
(*#line 252.26 "syntax-io.ioml"*)
(*#line 252.26 "syntax-io.ioml"*)IOInt32.readInt 
(*#line 1266.34 "syntax-io.sml"*)p of NONE  => NONE  | SOME y0  => (case IOTypes.readInt p of NONE  => NONE  | SOME y1  => (SOME (y0 , y1 )))of NONE  => NONE  | SOME x  => SOME (BatchTxout x )) | _  => NONE 
(*#line 253.1 "syntax-io.ioml"*)end 
