package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeBooleanConst(val value: Boolean) extends AST
{
    vtype = classOf[Boolean]
    override def assemble(generator: GeneratorAdapter): Unit = generator.push(value)
}
