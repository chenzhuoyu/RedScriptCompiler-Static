package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeIf(val expr: NodeExpr, val success: NodeStatement, val failed: Option[NodeStatement]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
