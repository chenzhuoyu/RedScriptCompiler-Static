package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeRange(val lower: NodeExpr, val upper: NodeExpr) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
