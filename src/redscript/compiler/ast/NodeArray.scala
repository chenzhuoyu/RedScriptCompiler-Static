package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeArray(val items: List[NodeExpr]) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
