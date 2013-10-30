
import sun.misc.Unsafe

// @see http://mishadoff.github.io/blog/java-magic-part-4-sun-dot-misc-dot-unsafe/

val f = classOf[Unsafe].getDeclaredField("theUnsafe")
f.setAccessible(true)
val unsafe = f.get(null).asInstanceOf[Unsafe]

