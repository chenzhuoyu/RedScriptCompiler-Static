package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeName(val name: String, val isGet: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
