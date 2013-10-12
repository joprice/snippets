
public class LazyInit {
  // lazy load static value - "holder class idiom"

  private static String loadValue(String method) {
    System.out.println("value loaded by " + method);
    return "test";
  }

  // value is initialized once by jvm - synchronized only on init
  private static class FieldHolder {
    static final String field = loadValue("static");
  }

  static String getFieldStatic() {
    return FieldHolder.field;
  }

  // lazy load instance method - "double-check idiom"

  private volatile String field;

  String getFieldInstance() {
    // copy to local to avoid another read
    String result = field;
    if (result == null) {
      // lock and check again
      synchronized(this) {
        result = field;
        if (result == null) {
          // must be stored in this order - or elese requires multiple field stores
          field = result = loadValue("instance");
        }
      }
    }
    return result;
  }

  public static void main(String[] args) {
    for (int i = 0; i < 5; i++) {
      System.out.println(getFieldStatic());
    }

    LazyInit l = new LazyInit();
    for (int i = 0; i < 5; i++) {
      System.out.println(l.getFieldInstance());
     }
  }
}

