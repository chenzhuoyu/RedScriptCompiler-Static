package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeName(val name: String) extends AST
{
    var isGet = false
    override def assemble(method: MethodVisitor): Unit = ()
}
