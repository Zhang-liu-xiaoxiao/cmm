package common;/**
 * @Author: zlxx
 * @Date: 2020/12/4 23:01
 */

/**
 * @Author: zlxx
 * @Date: 2020/12/4 23:01
 */

public class Word {
    // 单词序号
    private int id;
    // 单词的值
    private String value;

    // 单词类型
    private int type;

    // 单词所在行
    private int line;

    //单词是否合法
    boolean isLegal = true;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getLine() {
        return line;
    }

    public void setLine(int line) {
        this.line = line;
    }

    public boolean isLegal() {
        return isLegal;
    }

    public void setFlag(boolean flag) {
        this.isLegal = flag;
    }

    public Word() {
    }

    public String toStringWithLine() {
        switch (type) {
            case Compiler.IF: return "LINE." + this.line + ": IF";
            case Compiler.ELSE: return "LINE." + this.line + ": ELSE";
            case Compiler.WHILE: return "LINE." + this.line + ": WHILE";
            case Compiler.INT: return "LINE." + this.line + ": INT";
            case Compiler.PLUS: return "LINE." + this.line + ": +";// return "PLUS";
            case Compiler.MINUS: return "LINE." + this.line + ": -";// return "MINUS";
            case Compiler.MUL: return "LINE." + this.line + ": *";// return "MUL";
            case Compiler.DIV: return "LINE." + this.line + ": /";// return "DIV";
            case Compiler.ASSIGN: return "LINE." + this.line + ": =";// return "ASSIGN";
            case Compiler.LT: return "LINE." + this.line + ": <";// return "LT";
            case Compiler.EQ: return "LINE." + this.line + ": ==";// return "EQ";
            case Compiler.NOTEQ: return "LINE." + this.line + ": <>";// return "NEQ";
            case Compiler.LPARENT: return "LINE." + this.line + ": (";// return "LPARENT";
            case Compiler.RPARENT: return "LINE." + this.line + ": )";// return "RPARENT";
            case Compiler.SEMICOLONS: return "LINE." + this.line + ": ;";// return "SEMI";
            case Compiler.LBRACE: return "LINE." + this.line + ": {";// return "LBRACE";
            case Compiler.RBRACE: return "LINE." + this.line + ": }";// return "RBRACE";
            case Compiler.LBRACKET: return "LINE." + this.line + ": [";// return "LBRACKET";
            case Compiler.RBRACKET: return "LINE." + this.line + ": ]";// return "RBRACKET";
            case Compiler.LET: return "LINE." + this.line + ": <=";// return "LET";
            case Compiler.GT: return "LINE." + this.line + ": >";// return "GT";
            case Compiler.GET: return "LINE." + this.line + ": >=";// return "GET";
            case Compiler.ID:// return "ID";
            case Compiler.LITERAL_INT:// return "LITERAL_INT";
            case Compiler.LITERAL_FLOAT: return "LINE." + this.line + ": " + this.value;// return "LITERAL_REAL";
            case Compiler.MAIN: return "LINE." + this.line + ": " + this.value;// return "LITERAL_REAL";

            default: return "LINE." + this.line + ": UNKNOWN";
        }
    }

    @Override
    public String toString() {
        switch (type) {
            case Compiler.IF: return "IF";
            case Compiler.ELSE: return "ELSE";
            case Compiler.WHILE: return "WHILE";
            case Compiler.INT: return "INT";
            case Compiler.PLUS: return "+";// return "PLUS";
            case Compiler.MINUS: return "-";// return "MINUS";
            case Compiler.MUL: return "*";// return "MUL";
            case Compiler.DIV: return "/";// return "DIV";
            case Compiler.ASSIGN: return "=";// return "ASSIGN";
            case Compiler.LT: return "<";// return "LT";
            case Compiler.EQ: return "==";// return "EQ";
            case Compiler.NOTEQ: return "<>";// return "NEQ";
            case Compiler.LPARENT: return "(";// return "LPARENT";
            case Compiler.RPARENT: return ")";// return "RPARENT";
            case Compiler.SEMICOLONS: return ";";// return "SEMI";
            case Compiler.LBRACE: return "{";// return "LBRACE";
            case Compiler.RBRACE: return "}";// return "RBRACE";
            case Compiler.LBRACKET: return "[";// return "LBRACKET";
            case Compiler.RBRACKET: return "]";// return "RBRACKET";
            case Compiler.LET: return "<=";// return "LET";
            case Compiler.GT: return ">";// return "GT";
            case Compiler.GET: return ">=";// return "GET";
            case Compiler.ID:// return "ID";
            case Compiler.LITERAL_INT:// return "LITERAL_INT";
            case Compiler.LITERAL_FLOAT: return "" + this.value;// return "LITERAL_REAL";
            case Compiler.MAIN: return "MAIN" + this.value;// return "LITERAL_REAL";
            case Compiler.VOID: return "VOID" + this.value;// return "LITERAL_REAL";


            default: return "UNKNOWN";
        }
    }

    public Word(int id, String value, int type, int line, boolean flag) {
        this.id = id;
        this.value = value;
        this.type = type;
        this.line = line;
        this.isLegal = flag;
    }
}
