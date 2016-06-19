package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeStringConst(val value: String) extends AST
{
    vtype = classOf[String]
    override def assemble(generator: GeneratorAdapter): Unit = generator.push(value)
}
