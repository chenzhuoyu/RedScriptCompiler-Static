package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeAttr(val name: NodeName, val isGet: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
