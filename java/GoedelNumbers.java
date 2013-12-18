
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;
import java.util.Arrays;
import java.math.BigInteger;
import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.SortedMap;

/*
 * Converts a string to a Goedel number then factorizes the number back into its primes.
 * A Goedel number is the product of a series of primes starting with 2, each raised to the power 
 * of a unique number representing an input symbol. In this case, I used the Unicdoe codepoint 
 * for each character. The numbrs get very large. Running the below example produces a number with over 16000 digits
 *
 * java GoedelNumbers 'class Test { public static void main(String[] args) { System.out.println("test") } }'
 */
public class GoedelNumbers {

  public static void main(String[] args) {
    if (args.length > 0 && args[0].length() > 0) {
      testGoedelNumber(args[0]);
    } else {
      System.out.println("Please provide a string to convert");
    }
  }

  private static void testGoedelNumber(String s) {
    System.out.print("Code points: ");
    CodePointIterator ci = new CodePointIterator(s);
    while (ci.hasNext()) {
      System.out.print(ci.next() + " ");
    }
    System.out.println();

    System.out.print("Primes: ");
    PrimeIterator pi = new PrimeIterator();
    for (int i = 0; i < s.length(); i++) {
      System.out.print(pi.next() + " ");
    }
    System.out.println();

    BigInteger g = goedelNumber(s);
    System.out.println("Goedel number: " + g);

    List<BigInteger> fs = primeFactors(g);

    // group by prime to get exponent
    Map<BigInteger, Integer> distinct = countDistinct(fs);
    SortedMap<BigInteger, Integer> counts = new TreeMap<BigInteger, Integer>();
    counts.putAll(distinct);

    System.out.println("Prime factors (prime ^ codepoint): ");
    for (BigInteger prime: counts.keySet()) {
      int e = counts.get(prime);
      char letter = (char) e;
      System.out.println("\t" + letter + "\t" + prime + " ^ " + e + " ");
    }
  }

  private static <T> Map<T, Integer> countDistinct(Iterable<T> it) {
    Map<T, Integer> counts = new HashMap<T, Integer>();
    for (T i: it) {
      Integer c;
      if ((c = counts.get(i)) != null) {
        c += 1;
      } else {
        c = 1;
      }
      counts.put(i, c);
    }
    return counts;
  }

  public static BigInteger goedelNumber(String s) {
    CodePointIterator ci = new CodePointIterator(s);
    PrimeIterator pi = new PrimeIterator();
    BigInteger total = BigInteger.ONE;
    while (ci.hasNext()) {
      int c = ci.next();
      int p = pi.next();
      BigInteger n = BigInteger.valueOf(p).pow(c);
      total = total.multiply(n);
    }
    return total;
  }

  public static List<BigInteger> primeFactors(BigInteger number) {
    BigInteger n = number;
    List<BigInteger> factors = new ArrayList<BigInteger>();
    
    BigInteger i = BigInteger.valueOf(2); 
    while (i.compareTo(n) < 1) { 
      while (n.mod(i).equals(BigInteger.ZERO)) {
        factors.add(i);
        n = n.divide(i);
      }
      i = i.add(BigInteger.ONE);
    }
    return factors;
  }
} 

/*
 * Adapted from http://stackoverflow.com/questions/1029897/comparing-a-char-to-a-code-point
 * */
class CodePointIterator implements Iterator<Integer> {

  public CodePointIterator(String sequence) {
    this.sequence = sequence;
  }

  // no-op
  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean hasNext() {
    return index < sequence.length();
  }

  @Override
  public Integer next() {
    int codePoint = sequence.codePointAt(index); 
    index += Character.charCount(codePoint); 
    return codePoint; 
  }

  private final String sequence;

  private int index = 0;
}

// iterator for infinite stream of primes
class PrimeIterator implements Iterator<Integer> {

  public PrimeIterator() {
    // default size of array
    this(10000, 2);
  }

  public PrimeIterator(int size, int resizeFactor) {
    this.resizeFactor = resizeFactor;
    primes = new boolean[size];
    resize(size);
  }

  // no-op
  @Override
  public void remove() {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean hasNext() {
    return true;
  }

  @Override
  public Integer next() {
    do {
      cur++;
      if (cur >= primes.length) {
        System.out.println("Reached max length. resizing...");
        resize(primes.length * resizeFactor);
      }
    } while (!primes[cur]);
    return cur;
  }

  private void resize(int size) {
    // get size before resizing
    int oldSize = primes.length;
    // resize
    primes = Arrays.copyOf(primes, size);

    //fill empty slots with true
    int from = size - oldSize;
    int to = size - 1;
    Arrays.fill(primes, from, to, true);        // assume all integers are prime.

    // run sieve to mark non-primes as false
    fillSieve(from, to);
  }

 /*
  * Adapted from http://www.mkyong.com/java/how-to-determine-a-prime-number-in-java/
  **/
  private void fillSieve(int from, int to) {
    primes[0] = primes[1] = false;       // we know 0 and 1 are not prime.

    for (int i=from; i < to; i++) {
      // if the number is prime, then go through all its multiples and make their values false.
      if (primes[i]) {
        for (int j = 2; i*j < primes.length; j++) {
          primes[i*j]=false;
        }
      }
    }
  }

  // array for sieve
  private boolean[] primes;

  // number of current prime
  private int cur = 0;

  private final int resizeFactor;
}

