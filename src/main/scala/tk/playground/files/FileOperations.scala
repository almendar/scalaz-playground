package tk.playground.files;

import java.io.File
import scalaz.{Free, Functor,Id,MonadPlus}
import scalaz.Free._



sealed trait Cursor
case class TopLevelCursor(fileName:String) extends Cursor
case class InnerClassCursor(klass:String) extends Cursor
case class InnerMethodCursor(methodName:String) extends Cursor


trait FileOperationF[+A] {
	def map[B](f:A=>B) : FileOperationF[B]
}

case class StartNewFile[+A](fileName:String, next: TopLevelCursor => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}


/* Top level instructions */
case class AddPackage[+A](cursor:TopLevelCursor, packageName:String,next: TopLevelCursor => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}

case class AddImports[+A](cursor:TopLevelCursor,imports:List[String],next: TopLevelCursor => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}


case class StartNewClass[+A](cursor : TopLevelCursor, klass:String, next: InnerClassCursor => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}

case class EndClass[+A](cursor : InnerClassCursor, next: TopLevelCursor => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}



/* Class level instructions */


case class WriteOutFile[+A](cursor:TopLevelCursor, outFile:File,next : Boolean => A) extends FileOperationF[A] {
	def map[B](f:A=>B) : FileOperationF[B] = copy(next = next andThen f)
}

case object ConsoleWriter {
	def step[A](action: FileOperationF[Free[FileOperationF,A]]): Unit = action match {
    	case _ => println("Hello")
	}
}






object FileOperationF  {

	def id[A] =  (x:A) => x

type FileOperation[A] = Free[FileOperationF, A]
	implicit val fileOpsFunctor = new Functor[FileOperationF] {
		def map[A,B](fa: FileOperationF[A])(f: A => B): FileOperationF[B] = fa map f
	}
	def startNewFile(fileName:String) : FileOperation[TopLevelCursor] = Free.liftF(StartNewFile(fileName,x=>x))
	def addPackage(cursor:TopLevelCursor ,packageName : String) : FileOperation[TopLevelCursor] = 
		Free.liftF(AddPackage(cursor,packageName,x => x))
	def addImports(cursor:TopLevelCursor,imports:List[String]) : FileOperation[TopLevelCursor] = Free.liftF(AddImports(cursor,imports,x => x))
	def writeOutFile(cursor: TopLevelCursor, file:File) : Free[FileOperationF,Boolean] = Free.liftF(WriteOutFile(cursor,file,x => x))
	def startClass(cursor:TopLevelCursor, klass:String) : FileOperation[InnerClassCursor] = Free.liftF(StartNewClass(cursor,klass,id))
	def endClass(cursor:InnerClassCursor) : FileOperation[TopLevelCursor] = Free.liftF(EndClass(cursor,id))

}



trait Interpreter


object ConsoleInterpreter extends Interpreter {

	import FileOperationF._

	def interpretPure(kvs: FileOperation[Boolean],sb:StringBuilder): Unit = kvs.resume.fold({
		case StartNewFile(fileName,func) => println("New file started"); interpretPure(func(TopLevelCursor(fileName)),sb) 
		case AddPackage(cursor,packName,func) => sb.append("package " + packName+"\n"); interpretPure(func(cursor),sb)
		case AddImports(cursor,imports,func) =>  sb.append(imports.map(p => s"import ${p}").mkString("\n")); interpretPure(func(cursor),sb)
		case WriteOutFile(cursor, write,func) => println(sb.toString); func(true)
		case StartNewClass(cursor,klass,func) => sb.append(s"\n class $klass {\n"); interpretPure(func(InnerClassCursor(klass)),sb)
		case EndClass(cursor,func) => sb.append("\n}\n"); interpretPure(func(new TopLevelCursor("Return from klass")),sb)
		}, 
		_ => ())
} 


object Test extends App {
import FileOperationF._
val p : FileOperation[Boolean] = for {
	topLevel <- startNewFile("Service.scala")
	topLevel <- addPackage(topLevel,"com.pl.tk")
	topLevel <- addImports(topLevel,List("java.io.File"))
	InnerClassCursor <- startClass(topLevel,"Foo")
	topLevel <- endClass(InnerClassCursor)
	f <- writeOutFile(topLevel, new File("./out.scala"))
} yield f


	ConsoleInterpreter.interpretPure(p, new StringBuilder)

}


