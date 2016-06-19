package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeRTuple(val items: List[Either[NodeRTuple, NodeExpr]]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeLTuple(val items: List[Either[NodeLTuple, NodeValue]]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
