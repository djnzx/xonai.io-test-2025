package xonai;

import java.util.Arrays;

/**
 * Code generation of SQL query ====================================================================
 *
 * This exercise is a simplified example of the type of logic one needs to write when targeting
 * XONAI underlying compiler to accelerate big data computations. Data is presented in columnar
 * format (as opposed to default Spark row format) similarly to the format used by our engine. The
 * goal is to write the missing code that performs the computation of the query.
 *
 * ATTENTION:
 * It should be written with performance in mind where every extra instruction matters. Also use of
 * libraries or JDK classes/methods are discouraged as the role entails writing low-level libraries.
 * Feel free to create new functions but KEEP existing ones.
 *
 * Consider the following SQL query:
 *
 *   select
 *     100 * (sum(price * discount) / sum(price)) as discount_ratio,
 *   	 (avg(price) filter(where discount = 0.05)) as avg_price
 *   from
 *   	 item
 *   where (
 *   	 discount between .05 and .07
 *   	 and quantity < 24
 *   	 and status = `A`
 *   ) or comment LIKE `PROMO%SUMMER`
 *
 * It is composed by 3 main tasks:
 *   1. Read input
 *   2. Filter by WHERE condition
 *   3. Aggregate
 *
 * Traditionally data is represented in row format:
 *
 *   --------------------------------------------------------
 *    RowId   Quantity   Price   Discount   Status   Comment
 *   --------------------------------------------------------
 *    0       6          19.9    0.07       "A"      "PROMO"
 *   --------------------------------------------------------
 *    1       18         24.9    0.04       "A"      ""
 *   --------------------------------------------------------
 *    2       6          9.9     0.08       "AR"     ""
 *   --------------------------------------------------------
 *
 * This program represents data in columnar format:
 *
 *   -------------------------------------
 *    RowId      0         1         2
 *   -------------------------------------
 *    Quantity   6         18        6
 *   -------------------------------------
 *    Price      19.9      24.9      9.9
 *   -------------------------------------
 *    Discount   0.07      0.04      0.08
 *   -------------------------------------
 *    Status     "A"       "A"       "AR"
 *   -------------------------------------
 *    Comment    "PROMO"   ""        ""
 *   -------------------------------------
 */
public class Main {

  public static class InputBatch {
    public int numRows;
    public int[] quantity;
    public double[] price;
    public double[] discount;
    public StringColumn status = new StringColumn();
    public StringColumn comment = new StringColumn();
  }

  public static class FilteredBatch {
    public int numRows;
    public int[] quantity;
    public double[] price;
    public double[] discount;
    public StringColumn status = new StringColumn();
    public StringColumn comment = new StringColumn();
  }

  public static class AggregatedBatch {
    public int numRows;
    public double[] discount_ratio;
    public double[] avg_price;
  }

  /**
   * Variable length ASCII string.
   */
  public static class StringColumn {
    public int[] offset; // start of string in `buffer` (for each row)
    public int[] length; // lengths (for each row)
    public byte[] buffer; // buffer with data of all strings

    // Example: 2 strings - "Hi" and "there"
    //
    // offset  |  0  2
    // length  |  2  5
    // buffer  |  H  i  t  h  e  r  e
  }

  public static void main(String[] args) {
    testIsEqualToA();
    testIsLikePromoSummer();
    testQuery();
  }

  public static void testQuery() {
    InputBatch input = new InputBatch();

    input.numRows = 10;
    input.quantity = new int[]{6, 18, 6, 30, 24, 12, 18, 6, 24, 12};
    input.price = new double[]{19.9d, 24.9d, 9.9d, 14.9d, 9.9d, 19.9d, 24.9d, 19.9d, 9.9d, 14.9};
    input.discount = inputDiscount();

    input.status.offset = new int[]{0, 1, 2, 4, 5, 6, 7, 8, 9, 10};
    input.status.length = new int[]{1, 1, 2, 1, 1, 1, 1, 1, 1, 1};
    input.status.buffer = "AAARNAANAAA".getBytes();

    input.comment.offset = new int[]{0, 0, 0, 5, 0, 0, 0, 0, 20, 0};
    input.comment.length = new int[]{5, 0, 0, 15, 5, 0, 0, 0, 12, 5};
    input.comment.buffer = "PROMOPROMO IN SUMMERPROMO WINTER".getBytes();

    FilteredBatch filtered = filter(input);
    print(filtered);
    AggregatedBatch aggregated = aggregate(filtered);

    assertResult(5.8893854d, aggregated.discount_ratio[0], "discount ratio");
    assertResult(16.566666d, aggregated.avg_price[0], "avg price");
  }

  public static double[] inputDiscount() {
    // Possible discount values.
    double[] dictionary = {0.04d, 0.05d, 0.07d, 0.08d};
    // Contains for each row the index in the discount dictionary.
    int[] ids = {2, 0, 3, 1, 0, 2, 2, 1, 3, 1};
    double[] discounts = new double[ids.length];
    for (int i = 0; i < ids.length; i++) {
      discounts[i] = dictionary[ids[i]];
    }
    return discounts;
  }

  public static boolean stringColumnIsValid(StringColumn str) {
    if (str == null || str.offset == null || str.length == null || str.buffer == null) return false;
    return str.offset.length == str.length.length;
  }

  public static boolean rowIdInRange(int rowId, int size) {
    return rowId >= 0 && rowId < size;
  }

  // Check if string is equal to "A" at given row id.
  // Not using String is preferred.
  public static boolean isEqualToA(int rowId, StringColumn str) {
    if (!stringColumnIsValid(str)) return false;
    if (!rowIdInRange(rowId, str.buffer.length)) return false;

    if (str.length[rowId] != 1) return false;
    int rowOff = str.offset[rowId];
    return str.buffer[rowOff] == 'A';
  }

  // done in Sandbox.scala
  public static void testIsEqualToA() {}

  // Check if comment is like "PROMO%SUMMER" at given row id.
  // Not using String and regex is preferred.
  static byte[] pattern = "PROMO%SUMMER".getBytes();
  static byte wild = '%';

  public static boolean isLikePromoSummer(int rowId, StringColumn str) {
    if (!stringColumnIsValid(str)) return false;
    if (!rowIdInRange(rowId, str.buffer.length)) return false;
    int len = str.length[rowId];
    if (len < pattern.length - 1) return false;
    int base = str.offset[rowId];
    byte[] bb = str.buffer;
    // left
    int i = base; // buffer
    int j = 0;    // pattern
    while (pattern[j] != wild) if (bb[i++] != pattern[j++]) return false;
    // right
    i = base + len - 1;
    j = pattern.length - 1;
    while (pattern[j] != wild) if (bb[i--] != pattern[j--]) return false;
    // found
    return true;
  }

  // done in Sandbox.scala
  public static void testIsLikePromoSummer() {}

  /**
   * Applies filter: (
   *                 discount between .05 and .07
   *                 and quantity < 24
   *                 and status = `A`
   *                 ) or comment LIKE `PROMO%SUMMER`
   */
  public static FilteredBatch filter(InputBatch input) {
    int[] filtered1 = new int[input.numRows];
    int size1 = 0;
    for (int i = 0; i < input.numRows; i++) {
      double discount = input.discount[i];
      if (discount >= .05 && discount <= .07) filtered1[size1++] = i;
    }
//    System.out.printf("%s - %s\n",Arrays.toString(filtered1), size1);

    int[] filtered2 = new int[size1];
    int size2 = 0;
    for (int i = 0; i < size1; i++) {
      if (input.quantity[filtered1[i]] < 24) filtered2[size2++] = filtered1[i];
    }
//    System.out.printf("%s - %s\n",Arrays.toString(filtered2), size2);

    int[] filtered3 = new int[size2]; // we could reuse filtered1
    int size3 = 0;
    for (int i = 0; i < size2; i++) {
      if (isEqualToA(filtered2[i], input.status)) filtered3[size3++] = filtered2[i];
    }
//    System.out.printf("%s - %s\n", Arrays.toString(filtered3), size3);

    // OR

    int[] filtered4 = new int[input.numRows];
    int size4 = 0;
    for (int i = 0; i < input.numRows; i++) {
      if (isLikePromoSummer(i, input.comment)) filtered4[size4++] = i;
    }
//    System.out.printf("%s - %s\n", Arrays.toString(filtered4), size4);

    // merge filtered3 and filtered4 into filtered5
    // we can eliminate this extra allocation,
    // but it will increase mental load in final check/copy
    int[] filtered5 = new int[size3 + size4];
    int i3 = 0; // filtered3
    int i4 = 0; // filtered4
    int i5 = 0; // filtered5

    // merge possibly duplicates
    while (i3 < size3 && i4 < size4) {
      if (filtered3[i3] < filtered4[i4]) {
        filtered5[i5++] = filtered3[i3++];
      } else if (filtered3[i3] > filtered4[i4]) {
        filtered5[i5++] = filtered4[i4++];
      } else {
        filtered5[i5++] = filtered4[i4++]; i3++;
      }
    }
    // copy tails
    if (size3 - i3 > 0) {
      System.arraycopy(filtered3, i3, filtered5, i5, size3 - i3);
      i5 += size3 - i3;
    }
    if (size4 - i4 > 0) {
      System.arraycopy(filtered4, i4, filtered5, i5, size4 - i4);
      i5 += size4 - i4;
    }
//    System.out.printf("%s - %s\n", Arrays.toString(filtered5), size5);

    // final copy
    FilteredBatch output = new FilteredBatch();
    output.numRows = filtered5.length;
    output.quantity = new int[i5];
    output.price = new double[i5];
    output.discount = new double[i5];

    for (int i = 0; i < i5; i++) {
      int idx = filtered5[i];
      output.quantity[i] = input.quantity[idx];
      output.price[i] = input.price[idx];
      output.discount[i] = input.discount[idx];
    }

    output.status = copyAtIndexes(input.status, filtered5, i5);
    output.comment = copyAtIndexes(input.comment, filtered5, i5);

    return output;
  }

  public static StringColumn copyAtIndexes(StringColumn in, int[] indexes, int count) {
    var outcome = new StringColumn();
    outcome.length = new int[count];
    outcome.offset = new int[count];
    int buf_size = 0;
    for (int i = 0; i < count; i++) {
      int l = in.length[indexes[i]];
      buf_size += l;
      outcome.length[i] = l;
      outcome.offset[i] = i == 0 ? 0 : outcome.offset[i - 1] + l;
    }
    outcome.buffer = new byte[buf_size];

    int z = 0;
    for (int i = 0; i < count; i++) {
      int l = in.length[indexes[i]];
      System.arraycopy(in.buffer, in.offset[indexes[i]], outcome.buffer, z, l);
      z += l;
    }

    return outcome;
  }

  public static void pretty(StringColumn sc) {
    Pretty$.MODULE$.show(sc);
  }

  public static void print(FilteredBatch batch) {
    System.out.println("Filtered Batch:");
    System.out.println("-".repeat(50));
    System.out.printf("num rows:     %d\n", batch.numRows);
    System.out.printf("num quantity: %s\n", Arrays.toString(batch.quantity));
    System.out.printf("num price:    %s\n", Arrays.toString(batch.price));
    System.out.printf("num discount: %s\n", Arrays.toString(batch.discount));
    System.out.println("status buffer"); pretty(batch.status);
    System.out.println("comment buffer"); pretty(batch.comment);
    System.out.println("-".repeat(50));
  }

  /**
   * Compute:
   * - 100 * (sum(price * discount) / sum(price)) as discount_ratio
   * - (avg(price) filter(where discount = 0.05)) as avg_price
   */
  public static AggregatedBatch aggregate(FilteredBatch input) {
    AggregatedBatch output = new AggregatedBatch();
    output.numRows = 1;

    double sum_price = 0;
    double sum_discount = 0;

    int d05count = 0;
    double d05totalPrice = 0;

    for (int i = 0; i < input.numRows; i++) {
      double price = input.price[i];
      double discount = input.discount[i];

      sum_discount += price * discount;
      sum_price += price;

      if (Math.abs(discount - 0.05) < 0.000001d) {
        d05count++;
        d05totalPrice += price;
      }
    }

    output.discount_ratio = new double[]{ 100.0 * (sum_discount / sum_price) };
    output.avg_price = new double[]{ d05totalPrice/d05count};

    return output;
  }

  private static void assertResult(double expected, double actual, String description) {
    if (Math.abs(expected - actual) > 0.000001d) {
      throw new RuntimeException("Unexpected " + description);
    }
  }
}
