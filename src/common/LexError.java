package common;/**
 * @Author: zlxx
 * @Date: 2020/12/5 0:38
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/5 0:38
 */
public class LexError {
    private Word word;
    private int lineNumber;
    private String info;
    private int errorCount;

    public LexError(Word word, int lineNumber, String info, int errorCount) {
        this.word = word;
        this.lineNumber = lineNumber;
        this.info = info;
        this.errorCount = errorCount;
    }

    public Word getWord() {
        return word;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public String getInfo() {
        return info;
    }

    public int getErrorCount() {
        return errorCount;
    }
}
