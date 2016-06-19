package redscript.compiler.ast

import org.objectweb.asm.commons.GeneratorAdapter

class NodeIntConst(val value: Int) extends AST
{
    vtype = classOf[Int]
    override def assemble(generator: GeneratorAdapter): Unit = generator.push(value)
}
