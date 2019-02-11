(*idea from lecture making different type so can be contain in same list*)
datatype Val = 
    INT of int
    |STR of string 
    |NAME of string
    |Bol of bool
    |Err of string
    |Unit of string
    |FUNC of string list
    |IOFUN of string list
datatype command = 
    Add | Sub |Multi |Divi|Push  |Pop |Rem |Neg |Swap|Cat |And |Or |Not |Equal |Less| Bind | If
val x = "3"
val x2 = INT(2);
(*functional map *)
fun new () = fn (x:string) => Err(":error1:" ^ "\n"); 
fun add ((str:string) , (i:Val) , map:(string -> Val)) = fn x => if (x = str) then i else map (x);
(*fun addFun  ((str:string) , (i:Val) , map:(string -> Val)) = fn *)
fun delete ((str:string) ,  map:(string -> Val)) = fn x => if (x =str) then new() x else map(x);
(*this function eval the string and check what type it is *)
fun dontContainChar cL =
    case cL of
        [] => true
        |(x::xs) => if ((96 <(ord x)) andalso (123  >(ord x))) then false else dontContainChar (xs)
fun evalPush (s:string) = 

    let 
        val charL = explode s 
        fun f cL = 
        case cL of
        [] => Err (":error:")
        |(x::y::xs) => if ( Char.contains s #"\"") then STR ((String.substring(s , 1 , (String.size s) -3 ) )) (*check if s has quotation mark*)
                        else if ((96 <(ord x)) andalso (123  >(ord x)))  then  NAME (s )  (*check if s is a name by checking first char*)
                        else if ((64 <(ord x)) andalso (91  >(ord x)))  then  NAME (s )
                        else if (Char.contains s #".") then Err (":error:" ^ "\n" ) else if ( ((ord x) = 45 ) andalso ((96 <(ord y)) andalso (123  >(ord y)))) then Err (":error1:" ^ "\n" ) 
                        else if ((ord x) = 45) andalso not (dontContainChar (y::xs)) then Err (":error:" ^ "\n" ) (*check i s start with number but has char then would be invalid name*)
                        else if  ((ord x) = 45 ) andalso (not ((ord y)=48 )) then INT(~ (valOf(Int.fromString (String.implode (y::xs))))) 
                        else if ((ord x) = 45 ) andalso (((ord y)=48 ))  then INT (0) (*check cases start with negative sign*)
                        else if ((ord x) =58  )andalso ((ord y = 101)) then Err (":error:" ^ "\n" ) (*check if its a error*)
                        else if ((ord x) =58  ) then Bol(valOf(Bool.fromString(String.substring(s , 1 , (String.size s) -2 )))) (*else it would be bool*)
                        else if dontContainChar (x::y::xs) then  INT(valOf(Int.fromString s)) 
                        else Err (":error:" ^ "\n" )
        |(x::xs) =>if ((96 <(ord x)) andalso (123  >(ord x))) orelse ((64 <(ord x)) andalso (91  >(ord x))) then NAME(s) 
        else if dontContainChar (x::xs) then  INT(valOf(Int.fromString s))
        else Err (":error:" ^ "\n" )
                        
    in
        f charL
    end 
(*this function reads the string and determine what command should it use*)
fun readCom input =    if (String.isPrefix "push" input) then Push 
        else if (String.isPrefix "div" input) then  Divi
        else if (String.isPrefix "mul" input) then Multi
        else if (String.isPrefix "add" input) then Add
        else if (String.isPrefix "sub" input) then Sub
        else if (String.isPrefix "pop" input) then Pop
        else if (String.isPrefix "swap" input) then Swap
        else if (String.isPrefix "neg" input) then Neg
        else if (String.isPrefix "cat" input) then Cat
        else if (String.isPrefix "and" input) then And
        else if (String.isPrefix "or" input) then Or
        else if (String.isPrefix "not" input) then Not
        else if (String.isPrefix "equal" input) then Equal
        else if (String.isPrefix "lessThan" input) then Less
        else if (String.isPrefix "bind" input) then Bind
        else if (String.isPrefix "if" input) then If
        else Rem
            
(* use of different command*)
fun interpret (Sub, INT(x)::INT(y)::xs) = INT(y-x)::xs | interpret(Sub,z) = Err(":error:"^ "\n")::z
    |interpret (Add,INT(x)::INT(y)::xs) =INT(x+y)::xs |interpret(Add, z)  =Err (":error:" ^ "\n") :: z  
     |interpret (Pop, x::xs) = xs | interpret (Pop, z) = Err (":error:" ^ "\n")::z | interpret (Push , stack ) = stack 
     |interpret (Multi, INT(x)::INT(y)::xs) = INT(y*x)::xs | interpret(Multi,z) = Err(":error:" ^ "\n")::z 
     |interpret (Divi, INT(x)::INT(y)::xs) = if (x=0) then Err(":error:" ^ "\n")::INT(x)::INT(y)::xs else INT(y div x)::xs | interpret(Divi,z) = Err(":error:" ^ "\n")::z
     |interpret (Rem, INT(x)::INT(y)::xs) = if (x=0) then Err(":error:" ^ "\n")::INT(x)::INT(y)::xs else INT(y mod x)::xs | interpret(Rem,z) = Err(":error:2" ^ "\n")::z
     |interpret (Neg, INT(x)::xs) = INT(~x)::xs |interpret (Neg, z)= Err(":error:" ^ "\n")::z
     |interpret (Swap, x::y::xs) = y::x::xs| interpret (Swap , z) = Err(":error:" ^ "\n")::z
     |interpret (Cat, STR(x)::STR(y)::xs) = STR((y ^ x))::xs | interpret(Cat,z) = Err(":error:"^ "\n")::z
     |interpret (And, Bol(x)::Bol(y)::xs) = Bol((x andalso y))::xs | interpret(And,z) = Err(":error:"^ "\n")::z
     |interpret (Or, Bol(x)::Bol(y)::xs) = Bol((x orelse y))::xs | interpret(Or,z) = Err(":error:"^ "\n")::z
     |interpret (Not, Bol(x)::xs) = Bol(not x)::xs | interpret(Not,z) = Err(":error:"^ "\n")::z
     |interpret (Equal, INT(x)::INT(y)::xs) = Bol(x=y)::xs | interpret(Equal,z) = Err(":error:"^ "\n")::z
     |interpret (Less, INT(x)::INT(y)::xs) = Bol(y<x)::xs | interpret(Less,z) = Err(":error:"^ "\n")::z
     |interpret (Bind, Err(x)::NAME(y)::xs) = Err(":error:"^ "\n")::Err(x)::NAME(y)::xs |interpret (Bind,x::NAME(y)::xs) = Unit(":unit:" ^ "\n")::xs | interpret (Bind, z) = Err(":error:"^ "\n")::z
     |interpret (If,x::y::Bol(z)::xs) = if (z) then y::xs else x::xs| interpret(If,z) =Err(":error:"^ "\n")::z
fun changeMap (stack1, map)=
    case stack1 of 
        (Err(x)::NAME(y)::xs) => map
        |(NAME(x)::NAME(y)::xs) => add(y,map(x),map)
        |(x::NAME(y)::xs) => add(y,x,map)
        |_ => map
fun notFunctionCall (input,map) =
    case (input) of 
        (x::NAME(y)::xs) => (case map(y) of FUNC(a)=> false | IOFUN(a)=> false |_ =>true)
            | _ => true

(*check if the if the name need to be replaced by its value when given certain command*)
fun decideName (comm:command, stack, xA:Val,yA:Val)=
    case (comm,stack,xA,yA) of
        (Sub,x1::y1::xs,INT(x),INT(y)) => INT(x)::INT(y)::xs
        |(Add,x1::y1::xs,INT(x),INT(y)) => INT(x)::INT(y)::xs
        |(Multi,x1::y1::xs,INT(x),INT(y)) => INT(x)::INT(y)::xs
        |(Divi,x1::y1::xs,INT(x),INT(y)) => if (x=0) then stack else INT(x)::INT(y)::xs
        |(Rem,x1::y1::xs,INT(x),INT(y)) => if (x=0) then stack else INT(x)::INT(y)::xs
        |(Neg,x1::xs,INT(x),_) => INT(x)::xs
        |(Cat,x1::y1::xs,STR(x),STR(y)) => STR(x)::STR(y)::xs
        |(And,x1::y1::xs,Bol(x),Bol(y)) =>Bol(x)::Bol(y)::xs
        |(Or,x1::y1::xs,Bol(x),Bol(y)) =>Bol(x)::Bol(y)::xs
        |(Not,x1::xs,Bol(x),_) => Bol(x)::xs
        |(Equal,x1::y1::xs,INT(x),INT(y)) => INT(x)::INT(y)::xs
        |(Less,x1::y1::xs,INT(x),INT(y)) => INT(x)::INT(y)::xs
        |(_,_,_,_) => stack
fun checkCOmm(com) =
    case com of 
        If => true
        |_ => false
fun popName (stack) =
    case stack of
        (NAME(x)::xs)=> true
        |_ =>false
fun isFunctionOnTOP input =
    case input of
    (FUNC(x)::xs)=>true
    |(IOFUN(x)::xs)=>true
    |_ =>false    
fun deleteName (z) =
    case z of
        NAME(x)=> x
        |_ =>" "
(*check if name is on time, and get the value of the name *)
fun isNameOnTop (com, stack ,map) =  
    case (com,stack) of
        (If,x::y::NAME(z)::xs) => x::y::(map z)::xs 
        |(_,(NAME(x)::NAME(y)::xs)) => decideName(com,stack,map x , map y)
        |(_,NAME(x)::y::xs) => decideName(com,stack,map x , y)
        |(_,NAME(x)::xs) => decideName(com,stack,map x , Err(":Error"))
        | (_,x::NAME(y)::xs) =>decideName(com,stack, x , map y)
        |_ => stack
fun getFunName (want, input)=
    case input of
        [] => " "
        | (x::xs) => if (ord x = 32) then implode (rev want) else getFunName (x::want , xs)
fun getArgName (input)=
    case input of
        [] => " "
        | (x::xs) => if (ord x = 32) then implode (xs) else getArgName (xs)

fun getIOFunList (want,input) =
    case input of
        [] => want
        |(x::xs) =>if (String.isPrefix "return" x)  then rev ("IOreturn" ::want) (*if return is in the end*)else if (String.isPrefix "funEnd" x) then rev ("IOnoreturn"::want) (*check if return is not in the end*)else  getIOFunList(x::want,xs)  

fun getInputAfterFun input =
    case input of
        [] => input
        |(x::xs) => if (String.isPrefix "fun " x) then getInputAfterFun(getInputAfterFun xs) else if (String.isPrefix "funEnd" x) then xs else getInputAfterFun xs
fun nameToString x =
    case x of
        NAME(y)=> y
        |_ => "hello"
fun funListToList input =
    case input of
        FUNC (x) => x
        |IOFUN(x) => x
        |_ => ["Hi"]
fun getInnerFunList (want,input) =
    case input of 
        []=> want
        |(x::xs)=> if (String.isPrefix "funEnd" x) then x::want else getInnerFunList(x::want,xs)

fun getFunList (want,input) =
    case input of
            [] => want
            |(x::xs) => if (String.isPrefix "fun " x) then  getFunList(  getInnerFunList([],input)@want,  getInputAfterFun xs)  else if (String.isPrefix "return" x)  then rev ("return"::want) else if (String.isPrefix "funEnd" x) then rev ("noreturn"::want) (*check if return is not in the end*)else  getFunList(x::want,xs)

fun returnRemainInput (input)=
    case input of
        [] => input
        |(x::xs) => if (String.isPrefix "let" x) then returnRemainInput(returnRemainInput(xs)) else if (String.isPrefix "end" x) then xs else returnRemainInput xs

fun typeCastList input =
    case input of 
        [] => []
        |(x::xs) => STR(x):: (typeCastList xs )
fun getSecItem input = 
    case input of 
        (x::y::xs) => y
        |_ => Err":error:"
fun errorOnTop input = 
    case input of
        (Err (x)::xs)=> true
        |_ => false
fun turnStacktoOutput (input,stack ,map) =
        case input of 
        [] => stack
        | (x::xs)=> if (String.isPrefix "push" x) then turnStacktoOutput (xs ,(evalPush (String.extract (x , 5,NONE)) ::stack),map) 
        else if (String.isPrefix "bind" x)  then turnStacktoOutput( xs ,interpret (readCom x ,stack ),(changeMap (stack, map))) 
        (*else if ((String.isPrefix "pop" x) andalso (popName stack)) then turnStacktoOutput( xs ,interpret (readCom x ,stack ),delete(deleteName(List.hd (stack)),map))*)
         else if (String.isPrefix "call" x)then  if notFunctionCall (stack,map) then Err(":error:"^ "\n")::stack else if errorOnTop stack then  Err(":error:"^ "\n")::stack else (if popName (stack) then 
                                                 case (map(nameToString( List.nth (stack,1)))) of 
                                                         FUNC(lx)=>
                                                             turnStacktoOutput(xs, 
                                                                                (if popName ((turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, map(nameToString( hd stack)),map)))) then [hd stack] else ((turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, map(nameToString( hd stack)),map))))) 
                                                        @(List.drop(stack,2))
                                                        ,map) 
                                                     |IOFUN(lx) =>
                                                             turnStacktoOutput(xs, 
                                                                                  ( tl(turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, map(nameToString( hd stack)),map))))
                                                        @(List.drop(stack,2))
                                                        ,add (nameToString( hd stack),hd (turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, map(nameToString( hd stack)),map))),map )) 
                             else  case  (map(nameToString( List.nth (stack,1)))) of 
                                                         FUNC(lx)=>
                                                             turnStacktoOutput(xs, 
                                                                                (turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, hd stack,map)))
                                                        @(List.drop(stack,2))
                                                        ,map) 
                                                     |IOFUN(lx) =>
                                                             turnStacktoOutput(xs, 
                                                                                  ( tl(turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, hd stack,map))))
                                                        @(List.drop(stack,2))
                                                        ,add (hd lx,hd (turnStacktoOutput( tl lx, 
                                                                                stack, 
                                                                                add(hd lx, hd stack,map))),map )) )

        else if  (String.isPrefix "inOutFun " x) then turnStacktoOutput(  getInputAfterFun (xs) , Unit(":unit:" ^ "\n") ::stack, 
            
            add(getFunName([],explode (String.extract (x,9,NONE)))^"\n",IOFUN (getArgName (explode (String.extract (x,9,NONE)))::getIOFunList ([],xs) @[getArgName (explode (String.extract (x,9,NONE)))]),map))
        
        else if  (String.isPrefix "fun " x) then turnStacktoOutput(  getInputAfterFun (xs) , Unit(":unit:" ^ "\n") ::stack, 

            add(getFunName([],explode (String.extract (x,4,NONE)))^"\n",FUNC (getArgName (explode (String.extract (x,4,NONE)))::getFunList ([],xs)),map))
        else if (String.isPrefix "let" x) then  turnStacktoOutput (returnRemainInput xs,  (List.hd(turnStacktoOutput(xs,stack,map)))::stack,map) 
        else if (String.isPrefix "end" x) then stack 
                else if (String.isPrefix "IOnoreturn" x) then [map(hd xs)] (*if return is at the end then return a empty list bind with stack*)
        else if (String.isPrefix "IOreturn" x) then (if (popName(stack)) then [map (hd xs),(map(nameToString (hd stack)))]  else [map(hd xs),hd stack])  (*if return is in input, then return the head as a list and add to stack*)
        else if (String.isPrefix "noreturn" x) then [] (*if return is at the end then return a empty list bind with stack*)
        else if (String.isPrefix "return" x) then (if (popName(stack)) then (case (map(nameToString(hd stack))) of FUNC(x) => [hd stack] | IOFUN (x) => [hd stack] | _ =>[(map(nameToString (hd stack)))])  else [hd stack]) (*if return is in input, then return the head as a list and add to stack*)
        
    
        else   turnStacktoOutput( xs ,interpret (readCom x ,isNameOnTop(readCom x,stack,map)), map) 


fun readStrings ( line) = if (String.isPrefix "push" line) then (String.extract(line,5,NONE))  else  (line)

fun test li =
    case li of
    (*STR(a)::b => (print (a); test b)
    |INT(a)::b => (print (Int.toString a); test b)*)
    a::b => (print ((Char.toCString a) ^ Bool.toString(a= #"\"") ^ "\n"); test b)
    |[] => print ("END")
fun read (inFile) =
    let 
        val inStream = TextIO.openIn inFile
        val readLine = TextIO.inputLine inStream
         fun pr (rline) =
             case rline of
                 NONE => (print ("empty");TextIO.closeIn inStream)
                 |SOME (c) => (test (explode c);pr (TextIO.inputLine inStream))
    in 
        pr (readLine)
    end

fun interpreter (inFile, outFile) =
let
    val inStream = TextIO.openIn inFile
    val readLine = TextIO.inputLine inStream
    val map = new()
    val outStream = TextIO.openOut outFile;
        fun reader (rline: string option )=
            case rline of 
                NONE => []
                | SOME (c) => if (String.isPrefix "quit" c) then [] else ( c :: reader(TextIO.inputLine inStream))(*print (Int.toString(length l)^"run"*)
        fun writer stack =
            case stack of
                [] => (TextIO.closeOut outStream)
                |(INT(x)::xs) => (    if x < 0 then TextIO.output(outStream,"-" ^ Int.toString(0-x) ^ "\n") else TextIO.output(outStream,Int.toString(x) ^ "\n");writer(xs))
                | (STR(x)::xs)=>(TextIO.output(outStream, (x) ^ "\n") ;writer(xs))
                | (Bol(x)::xs)=>(TextIO.output(outStream, ":" ^Bool.toString(x) ^ ":" ^ "\n");writer(xs))
                |(Err(x)::xs)=> (TextIO.output(outStream, (x)) ;writer(xs))
                | (NAME(x)::xs) =>(TextIO.output(outStream, (x)) ;writer(xs))
                |(Unit(x)::xs) =>(TextIO.output(outStream, (x)) ;writer(xs))
in 
    (writer ((turnStacktoOutput ((reader (readLine)),[],map))); TextIO.closeIn inStream)

end


