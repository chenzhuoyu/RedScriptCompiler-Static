package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeIndex(val index: NodeExpr, val isGet: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
