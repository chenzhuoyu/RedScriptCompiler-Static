package redscript.compiler.ast

import org.objectweb.asm.MethodVisitor

class NodeValue(val value: AST, val modifiers: List[AST]) extends AST
{
    override def assemble(method: MethodVisitor): Unit = ()
}
