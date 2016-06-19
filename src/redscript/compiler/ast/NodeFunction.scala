package redscript.compiler.ast
import org.objectweb.asm.commons.GeneratorAdapter

class NodeFormalArg(val name: NodeName, val variadic: Boolean) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}

class NodeFunctionDef(val name: NodeName, val args: List[NodeFormalArg], val body: NodeStatement) extends AST
{
    override def assemble(generator: GeneratorAdapter): Unit = ()
}
