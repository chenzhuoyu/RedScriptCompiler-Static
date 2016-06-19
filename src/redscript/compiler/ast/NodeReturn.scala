package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeReturn(val expr: Either[NodeRTuple, NodeExpr]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
