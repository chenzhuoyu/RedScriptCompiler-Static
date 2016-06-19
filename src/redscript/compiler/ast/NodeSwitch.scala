package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeCase(val keys: NodeExpr, val body: NodeStatement) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeSwitch(val expr: NodeExpr, val cases: List[NodeCase], val default: Option[NodeCase]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
