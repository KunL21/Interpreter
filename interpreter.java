import java.awt.List;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Stack;

public class interpreter {
	
	/**
	 * since I wrote this project in SMl first, i felt its alot easier to wrote java part in a similar fashion to sml
	 * 
	 * So I created this inner class Val which i could store different types of entry in one list
	 * 
	 * this Val object take two argument, 1st one is a string stating its type, second one is its value
	 * 
	 * */
	public static class Val {
		private String _type;
		private Object _value;
		public Val (String type, Object value){
			this._type = type;
			this._value = value;
		}
		public Val(String type , Integer value){
			this._type = type;
			this._value = value;
		}
		public Val(String type , Boolean value){
			this._type = type;
			this._value = value;
		}
		
		public Val(String type , String value){
			this._type = type;
			this._value = value;
		}
		public Val (String type, LinkedList<String> value){
			this._type = type;
			this._value = value;
		}
		public boolean isFun(){
			return _type.equals("fun");
		}
		public boolean isBool (){
			return _type.equals("bool");
		}
		public boolean isInt (){
			return _type.equals("int");
		}
		public boolean isString (){
			return _type.equals("string");
		}

		public String first(){
			return _type;
		}
		public Object second(){
		return _value;	
		}
	}
			static HashMap <String,Val>  tempMap = new HashMap <String,Val>();
			static Stack <Val> _stack;
			static HashMap<String,Val> _map = new HashMap<String,Val>();
			public  static void interpreter (String input,String output) throws IOException{
				//System.out.println(Integer.parseInt("-123"));
				_stack = new Stack<Val>();
				try {
				BufferedReader bReader = new BufferedReader(new FileReader(input));
				BufferedWriter bWriter = new BufferedWriter(new FileWriter(output));
				 LinkedList<String> list = new LinkedList<String>();
				 String line ;
				 
				 while ( (line = bReader.readLine())!=null){
					 list.add(line);
				 }
				 
				 turnInputToStack(list,_stack,_map);
				 //System.out.println(_map.size());
				while (!_stack.isEmpty()){
					Val x = _stack.pop();
					if (x.first().equals("bool")){
					 bWriter.write(":" +x.second().toString() +":");
					}
					else if (x.first().equals("int")){
						bWriter.write(x.second().toString());
					}
					else {
					bWriter.write(x.second().toString());
					}
					bWriter.newLine();
				}
				 bWriter.close();
				 bReader.close();
				}
				catch (FileNotFoundException e) {
				   
					
				}
			}
			/**
			 * check what we push into stack
			 * @param s
			 */
			public static void evaluatePush (String s , Stack<Val> stack ){
			if (s.equals( ":true:") ){
				stack.push(new Val("bool",true));
				return;
			}
			if (s.equals( ":false:") ){
				stack.push(new Val("bool",false));
				return;
			}

			if (s.contains(".")){
				stack.push(new Val("err",":error:"));
				return;
			}
			if (s.charAt(0)=='"'){
				stack.push(new Val("string", s.substring(1, s.length()-1)));
				return;
			}
			if ( (64<s.charAt(0)&& s.charAt(0)<91)||(96<s.charAt(0)&& s.charAt(0)<123)){
				stack.push(new Val("name",s));
				return;
			}
			if (s.startsWith("-0")){
				stack.push(new Val ("int",0));
				return;
			}
			if (s.startsWith("-") && dontContainChar(s.substring(1)) ){
				stack.push(new Val("int",Integer.parseInt(s)));
				return;
			}
			if (dontContainChar(s)){
				stack.push(new Val("int",Integer.parseInt(s)));
				return;
			}
			
			stack.push(new Val ("err",":error:"));
			
			}
			
			/**
			 * poping the list untill it has the input after end when encounter let
			 * @param list
			 * @return
			 */
		public static LinkedList<String>  getInputAfterLet (LinkedList<String> list){
			if (list.isEmpty()){
				return list;
			}
			String s = list.pop();
			if (s.equals("let")){
				return getInputAfterLet(getInputAfterLet(list));
			}
			if (s.equals("end")){
				return list;
			}
			else {
				return getInputAfterLet(list);
			}
		}
		/**
		 * similar to get input after let above 
		 * @param list
		 * @return
		 */
		public static LinkedList<String> getInputAfterFun(LinkedList<String> list){
		if (list.isEmpty()){
			return list;
		}
		String s = list.pop();
		if (s.startsWith("fun ")){
			return getInputAfterFun(getInputAfterFun(list));
		}
		if (s.equals("funEnd")){
			return list;
		}
		else {
			return getInputAfterFun(list);
		}
		}
		/**
		 * similar to the above 
		 * @param list
		 * @return
		 */
		public static LinkedList<String> getInputAfterIOFun(LinkedList<String> list){
		if (list.isEmpty()){
			return list;
		}
		String s = list.pop();
		if (s.startsWith("inOutFun ")){
			return getInputAfterIOFun(getInputAfterIOFun(list));
		}
		if (s.equals("funEnd")){
			return list;
		}
		else {
			return getInputAfterIOFun(list);
		}
		}


			/**
			 * When i encounter let..end problem
			 * i realize i had to recursively going thru input file instead of while loop
			 * so i createdt this function that is similar to what i wrote in SML
			 * @param the input list
			 * @param stack
			 * @param map
			 * @return a stack result of the given input
			 */
		public static	Stack <Val>  turnInputToStack (LinkedList<String> list,Stack <Val> stack, HashMap <String,Val> map){
			if (list.isEmpty()){
			return stack;
			}
			else {
				 String s = list.pop();
				 if (s.equals("quit")){
					 return stack;//the end
				 }
				 else if (s.startsWith("push ")){
					 evaluatePush(s.substring(5),stack);
					 return turnInputToStack ( list, stack,  map);
					
				 }
				 else if (s.equals("let")){
					 /**
					  * make deep copy of stack, map and input
					  * and then stack add the output of let
					  * similar to my SML code
					  */
					 LinkedList<String> letInput = new LinkedList<String>();
					 letInput.addAll(list);
					// System.out.println("before pop" + letInput.size());
					 getInputAfterLet(list);
					// System.out.println("after pop" + letInput.size());
					 Stack<Val> tempStack = new Stack<Val> ();
					 tempStack.addAll(stack);
					 HashMap <String,Val> tempMap = new HashMap <String,Val>();
					 tempMap.putAll(map);	
					 turnInputToStack ( letInput, tempStack,  tempMap);
					 stack.add(tempStack.pop());
					 return turnInputToStack ( list, stack,  map);
				 }
				 else if (s.startsWith("fun ")){
					 LinkedList<String> funInput = new LinkedList<String>();	
					 funInput.addAll(list);
					 getInputAfterFun(list);
					 String funAndArg [];
					 funAndArg = s.split(" ");
					 funInput.add(0, funAndArg[2]);
					 Val  funVal = new Val ("fun",funInput);
					 map.put( funAndArg [1], funVal);
					 stack.add(new Val("unit",":unit:"));
					 return turnInputToStack (list,stack,map);
					
				 }
				 else if (s.startsWith("inOutFun ")){
					 LinkedList<String> funInput = new LinkedList<String>();	
					 funInput.addAll(list);
					 getInputAfterIOFun(list);
					 String funAndArg [];
					 funAndArg = s.split(" ");
					 funInput.add(0, funAndArg[2]);
					 Val  funVal = new Val ("IOfun",funInput);
					 map.put( funAndArg [1], funVal);
					 stack.add(new Val("unit",":unit:"));
					 return turnInputToStack (list,stack,map);
					
				 }

				 else if (s.startsWith("call")){
					 if (stack.size()<2){
						 stack.push(new Val ("err",":error:"));
					 }
					 Val top = stack.pop();
					 Val second =stack.pop();
					 String name = " ";
					 boolean nameIsArg = false;
					 if (top.first().equals("name")&map.containsKey(top.second())){
						 name = (String)top.second();
						 nameIsArg = true;
						 top = map.get(top.second());
					 }
					 if (second.first().equals("name")  && map.containsKey(second.second()) && map.get(second.second()).first().equals("fun") &&!top.first().equals("err")  ){
						 Stack<Val> tempStack = new Stack<Val> ();
						 tempStack.addAll(stack);
						 HashMap <String,Val>  tempMap = new HashMap <String,Val>();
						 tempMap.putAll(map);	
						 LinkedList<String> tempList = new LinkedList<String>();
						 tempList.addAll((LinkedList<String>) map.get(second.second()).second());
						 //System.out.println( (top.first() + top.second()));
						 //System.out.println(tempList);
						 tempMap.put( tempList.pop(), new Val (top.first(), top.second()));
						 tempStack = turnInputToStack (tempList, tempStack,  tempMap);
						 if (tempStack !=null){
							 Val temp = tempStack.pop();
//							 if (temp.first().equals("name")){
//								 if (!tempMap.get(temp.second()).first().equals("fun")&&!tempMap.get(temp.second()).first().equals("IOfun")){
//								 temp = tempMap.get(temp.second());
//								 }
//							 }
							 //System.out.println("run" + tempStack.peek().second());
							 
							 stack.add(temp);
							// System.out.println("run");
						 }
						 return  turnInputToStack (list,stack,map);

					 }
					 else  if (second.first().equals("name")  && map.containsKey(second.second()) && map.get(second.second()).first().equals("IOfun") &&!top.first().equals("err")  ){
						 Stack<Val> tempStack = new Stack<Val> ();
						 tempStack.addAll(stack);
						 //tempStack.add(0,new Val( "name",name));
						 HashMap <String,Val> tempMap = new HashMap <String,Val>();
						 tempMap.putAll(map);	
						 LinkedList<String> tempList = new LinkedList<String>();
						 tempList =	 (LinkedList<String>) map.get(second.second()).second();
						 String arg = tempList.pop();
						 tempMap.put( arg, new Val (top.first(), top.second()));
						 tempStack = turnInputToStack (tempList, tempStack,  tempMap);
						 if (nameIsArg){
							 map.put(name, tempMap.get(arg));
						 }

						 if (tempStack !=null){
							 stack.add(tempStack.pop());
							 //System.out.println("run");
						 }

						 return turnInputToStack(list,stack,map);
						 
						 
					 }
					 else {
						 stack.push(second);
							stack.push(top);
							stack.push(new Val ("err",":error:"));
							//System.out.println("run");
							return stack;
					 }
					 
				 }
				 else if (s.startsWith("funEnd")){
					 stack = null;
					 return stack;
					 
				 }
				 else if (s.startsWith("return")){
					 //System.out.println(stack.peek().second());
					 //System.out.println("returned");
					 Val top = stack.pop();
					 if (top.first().equals("name")&&!map.get(top.second()).first().equals("fun")&&!map.get(top.second()).first().equals("IOfun")){
						 
						 top = map.get(top.second());
						 //System.out.println(top.second());
						 stack.add(top);
					 }
					 else {
						 stack.add(top);
					 }
					 return stack;
				 }
				 else if (s.equals("end")){
					 //System.out.println("run");
					 return stack;
				 }
				 
				 
				 else {
					 checkCom(s,stack,map);
					 return turnInputToStack ( list, stack,  map);
					 //System.out.println();
				 }
			}
		}
		/**
		 * 
		 * @param input would be the command from the input.txt file,
		 * and see handle them accordingly, first check size, and then check value on the stack match the type of requirement
		 * if no push error, if yes, push the result
		 */
		public static void checkCom (String input, Stack <Val> stack, HashMap <String,Val> map){
			if (input.equals("pop")){
				if (stack.isEmpty()){
					stack.push(new Val ("err",":error:"));
				}
				else {
				stack.pop();
				}
				return;
			}
			if (input.equals("add")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt()&&top.first()=="int" ){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}
					//System.out.println(map.containsKey((String)second.second()) );
					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						int y= (int) second.second();
						int x = (int) top.second();
						stack.push(new Val ("int",x+y));
					}
				}
				return;
			}
			if (input.equals("sub")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() &&top.first()=="int"){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						int y= (int) second.second();
						int x = (int) top.second();
						stack.push(new Val ("int",y-x));
					}
				}
				return;
			}
			if (input.equals("mul")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt()&&top.first()=="int" ){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						int y= (int) second.second();
						int x = (int) top.second();
						stack.push(new Val ("int",y*x));
					}
				}
				return;
			}
			
			if (input.equals("div")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() && (int)map.get((String)top.second()).second()!=0){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() &&(int)map.get((String)top.second()).second()!=0&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt()&&top.first()=="int"&&(int)top.second()!=0 ){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
						return;
					}
					if ((int)top.second()==0){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
						return;
					}
					else {
						int y= (int) second.second();
						int x = (int) top.second();
						stack.push(new Val ("int",y/x));
					}
				}
				return;
			}
			//rem
			if (input.equals("rem")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() && (int)map.get((String)top.second()).second()!=0){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() &&(int)map.get((String)top.second()).second()!=0 &&second.first()=="int"){
							
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() &&top.first()=="int" &&(int)top.second()!=0){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
						return;
					}
					if ((int)top.second()==0){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
						return;
					}
					else {
						int y= (int) second.second();
						int x = (int) top.second();
						stack.push(new Val ("int",y%x));
					}
				}
				return;
			}
			//neg
			if (input.equals("neg")){
				if (stack.isEmpty()){
					stack.push(new Val ("err",":error:"));
				}
				
				
				else	 {
					Val top = stack.pop();
					if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if(!top.first().equals("int")){
					stack.push(new Val ("err",":error:"));
					}
					else {
					stack.add(new Val("int",- (int)top.second()));
				}
				}
				return;
			}
			if (input.equals("swap")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					stack.push(top);
					stack.push(second);
					}
			}
			if (input.equals("cat")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isString() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isString() ){
							top = new Val ("string", (String)map.get((String)top.second()).second());
							second = new Val ("string", (String)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isString() ){
							top = new Val ("string", (String)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isString() ){
							second = new Val ("string", (String)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="string"|second.first()!="string"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						String y=  (String)second.second();
						String x =  (String) top.second();
						stack.push(new Val ("string",y+x));
					}
				}
				return;
			}
			if (input.equals("and")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isBool() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isBool() ){
							top = new Val ("bool", (Boolean)map.get((String)top.second()).second());
							second = new Val ("bool", (Boolean)map.get((String)second.second()).second());
							}
						}
					}
					else { 
						if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isBool() ){
							top = new Val ("bool", (Boolean)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isBool() ){
							second = new Val ("bool", (Boolean)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="bool"|second.first()!="bool"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						Boolean y=  (Boolean)second.second();
						Boolean x =  (Boolean) top.second();
						stack.push(new Val ("bool",y & x));
					}
				}
				return;
			}
			if (input.equals("or")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
						if (top.first().equals("name")&&second.first().equals("name")){
						
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isBool() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isBool() ){
							top = new Val ("bool", (Boolean)map.get((String)top.second()).second());
							second = new Val ("bool", (Boolean)map.get((String)second.second()).second());
							}
						}
					}
					else { 
						if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isBool() ){
							top = new Val ("bool", (Boolean)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isBool() ){
							second = new Val ("bool", (Boolean)map.get((String)second.second()).second());
						}
					}
					}
					if(top.first()!="bool"|second.first()!="bool"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						Boolean y=  (Boolean)second.second();
						Boolean x =  (Boolean) top.second();
						stack.push(new Val ("bool",y | x));
					}
				}
				return;
			}
			//NOT
			if (input.equals("not")){
				if (stack.isEmpty()){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					if (top.first().equals("name")){
					if (map.containsKey((String)top.second()) && map.get((String)top.second()).isBool()){
						top = new Val ("bool", (Boolean)map.get((String)top.second()).second());
					}
					}
					if(!top.first().equals("bool")){
					stack.push(new Val ("err",":error:"));
				}
				else {
					stack.add(new Val("bool",!(Boolean)top.second()));
				}
			}
				return;
			}
			//EQUAL
			if (input.equals("equal")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt()&&top.first()=="int" ){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}

					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						int y=  (int)second.second();
						int x =  (int) top.second();
						stack.push(new Val ("bool",y == x));
					}
				}
				return;
			}
			//lessThan
			if (input.equals("lessThan")){
				if (stack.size()<2){
					stack.push(new Val ("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second = stack.pop();
					if (top.first().equals("name")&&second.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt() ){
							if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt() ){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
							}
						}
					}
					else { if (top.first().equals("name")){
						if (map.containsKey((String)top.second()) && map.get((String)top.second()).isInt()&&second.first()=="int"){
							top = new Val ("int", (Integer)map.get((String)top.second()).second());
						}
						
						}
					if (second.first().equals("name")){
						if (map.containsKey((String)second.second()) && map.get((String)second.second()).isInt()&&top.first()=="int" ){
							second = new Val ("int", (Integer)map.get((String)second.second()).second());
						}
					}
					}

					if(top.first()!="int"|second.first()!="int"){
						stack.push(second);
						stack.push(top);
						stack.push(new Val ("err",":error:"));
					}
					else {
						int y=  (int)second.second();
						int x =  (int) top.second();
						stack.push(new Val ("bool",y < x));
					}
				}
				return;
			}
			//BIND
			if (input.equals("bind")){
				 if (stack.size()<2){
					 stack.push(new Val("err",":error:"));
				 }
				 else {
					 Val top = stack.pop();
					 Val second = stack.pop();
					 
					 if (second.first().equals("name")&& top.first().equals("name") ){
						if (map.containsKey(top.second())){
							Val temp = map.get(top.second());
							map.put((String)second.second(), new Val(temp.first(),temp.second()));
							stack.push(new Val ("unit",":unit:"));
						}
						else {
							stack.push(second);
							stack.push(top);
							stack.push(new Val("err",":error:"));
						}
						return;
					 }
					 if (second.first().equals("name")){
						 if (top.first().equals("err")){
							 stack.push(second);
								stack.push(top);
								stack.push(new Val("err",":error:"));
						 }
						 else {
							 Object x = top.second();
							 //System.out.println("run");
							 map.put((String)second.second(), top); 
							 stack.push(new Val ("unit",":unit:"));
						 }	 
					 }
					 else {
						 stack.push(second);
							stack.push(top);
							stack.push(new Val("err",":error:"));
					 }
				 }
				 return;
			 }
			if (input.equals("if")){
				if (stack.size()<3){
					stack.push(new Val("err",":error:"));
				}
				else {
					Val top = stack.pop();
					Val second  =stack.pop();
					Val third = stack.pop();
					
					if (third.first().equals("name")){
						if (map.containsKey((String)third.second()) && map.get((String)third.second()).isBool() ){
							third = new Val ("bool", (Boolean)map.get((String)third.second()).second());
						}
					}
					if (!third.isBool()){
						stack.push(third);
						stack.push(second);
						stack.push(top);
						stack.push(new Val("err",":error:"));
					}
					else {
						if ((Boolean)third.second()){
							stack.push(second);
						}
						else {
							stack.push(top);
						}
					}
					
					
				}
			}

		}
	
		public static boolean dontContainChar (String input){
			for (int i=0;i<input.length();++i){
				if (!(47<input.charAt(i)&& input.charAt(i)<58)){
					return false;
				}
			}
			return true;
		}
			
}

