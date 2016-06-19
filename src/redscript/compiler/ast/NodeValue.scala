package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeValue(val value: AST, val modifiers: List[AST], val isRValue: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
