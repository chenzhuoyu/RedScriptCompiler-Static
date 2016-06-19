package redscript.compiler.ast

import java.math.BigInteger

import org.objectweb.asm.Type
import org.objectweb.asm.commons.{GeneratorAdapter, Method}

class NodeLongConst(val value: BigInteger) extends AST
{
    vtype = classOf[BigInteger]
    override def assemble(generator: GeneratorAdapter): Unit =
    {
        generator.newInstance(Type.getType(vtype))
        generator.push(value.toString(36))
        generator.push(36)
        generator.invokeConstructor(Type.getType(vtype), Method.getMethod(vtype.getConstructor(classOf[String], classOf[Int])))
    }
}
