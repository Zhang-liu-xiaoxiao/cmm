package common;/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:56
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/13 15:56
 */
public class FourCode {
    private String first;
    private String second;
    private String third;
    private String forth;

    private int number;

    public int getNumber() {
        return number;
    }
    public void setNumber(int number) {
        this.number = number;
    }
    public String getFirst() {
        return first;
    }
    public void setFirst(String first) {
        this.first = first;
    }
    public String getSecond() {
        return second;
    }
    public void setSecond(String second) {
        this.second = second;
    }
    public String getThird() {
        return third;
    }
    public void setThird(String third) {
        this.third = third;
    }
    public String getForth() {
        return forth;
    }
    public void setForth(String forth) {
        this.forth = forth;
    }

    public FourCode() {
    }

    public FourCode(int number, String first, String second, String third, String forth) {
        this.number = number;
        this.first = first;
        this.second = second;
        this.third = third;
        this.forth = forth;
    }
    @Override
    public String toString() {
        return String.format("(%s, %s, %s, %s) \n", first, second, third, forth);
    }

}
