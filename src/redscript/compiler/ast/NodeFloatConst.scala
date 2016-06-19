package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeFloatConst(val value: Double) extends AST
{
    vtype = classOf[Double]
    override def assemble(generator: GeneratorAdapter): Unit = generator.push(value)
}
