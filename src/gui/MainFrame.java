package gui;

import common.FourCode;
import common.InterpretException;
import common.ParseException;
import compile.Generator;
import compile.Interpreter;
import compile.LexAnalyse;
import compile.Parser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.LinkedList;

/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:07
 */
public class MainFrame extends JFrame {
    /**
     * 源文件的文本框
     */
    TextArea sourceFile;
    /**
     *源文件的路径
     */
    String sourcePath;
    String LL1Path;
    String wordListPath;
    String fourElementPath;
    LexAnalyse lexAnalyse;
    Parser parser;
    Generator generator;
    Interpreter interpreter;
    public MainFrame() {
        this.init();
    }
    public void init() {

        Toolkit toolkit = Toolkit.getDefaultToolkit();
        Dimension screen = toolkit.getScreenSize();
        setTitle("2018302120005-C语言小型编译器");
        setSize(750, 480);
        super.setResizable(false);
        super.setLocation(screen.width / 2 - this.getWidth() / 2, screen.height
                / 2 - this.getHeight() / 2);
        this.setContentPane(this.createContentPane());
    }
    private JPanel createContentPane() {
        JPanel p = new JPanel(new BorderLayout());
        p.add(BorderLayout.NORTH, createUpPane());
        p.add(BorderLayout.CENTER, createCenterPanel());
        p.add(BorderLayout.SOUTH, creatBottomPane());
        return p;
    }
    private Component createUpPane() {
        JPanel p = new JPanel(new FlowLayout());
        final FilePanel fp = new FilePanel("选择待分析文件");
        //final FilePanel fp1 = new FilePanel("xiang");
        JButton button = new JButton("确定");
        button.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                String text;
                try {
                    sourcePath = fp.getFileName();
                    text = readFile(sourcePath);
                    sourceFile.setText(text);

                } catch (IOException e1) {
                    e1.printStackTrace();
                }

            }
        });
        p.add(fp);
        //p.add(fp1);
        p.add(button);
        return p;
    }

    private Component createCenterPanel() {
        JPanel p = new JPanel(new BorderLayout());
        JLabel label = new JLabel("源文件如下：");
        sourceFile = new TextArea();
        sourceFile.setText("");
        p.add(BorderLayout.NORTH, label);
        p.add(BorderLayout.CENTER, sourceFile);
        return p;
    }

    private Component creatBottomPane() {
        JPanel p = new JPanel(new FlowLayout());
        JButton bt1 = new JButton("词法分析");
        JButton bt2 = new JButton("语法分析");
        JButton bt3 = new JButton("中间代码生成");
        JButton bt4 = new JButton("解释执行");
        bt1.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                lexAnalyse=new LexAnalyse(sourceFile.getText());
                try {
                    wordListPath = lexAnalyse.getWordList();
                } catch (IOException ioException) {
                    ioException.printStackTrace();
                }
                InfoFrame inf = new InfoFrame("词法分析", wordListPath);

                inf.setVisible(true);
            }
        });
        bt2.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                lexAnalyse=new LexAnalyse(sourceFile.getText());
                StringBuilder sb = null;
                parser=new Parser(lexAnalyse);
                try {
                    sb = parser.grammarAnalyse();
                } catch (ParseException parseException) {
                    parseException.printStackTrace();
                }
//                InfoFrame inf = new InfoFrame("语法分析", LL1Path);
                InfoFrame inf = new InfoFrame();
                inf.setTitle("语法分析");
                InfoFrame.text.setText(sb.toString());
                inf.setVisible(true);

            }
        });

        bt3.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                LinkedList<FourCode> fourCodes = null;
                try {
                    generator = new Generator(parser);
                    fourCodes = generator.generateCode();
                } catch (InterpretException parseException) {
                    parseException.printStackTrace();
                }
                InfoFrame inf = new InfoFrame();
                inf.setTitle("四元组生成");
                StringBuilder stringBuilder = new StringBuilder();
                for (int i=0;i<fourCodes.size();i++){
                    stringBuilder.append(i+":" +fourCodes.get(i).toString());
                }
                InfoFrame.text.setText(stringBuilder.toString());
                inf.setVisible(true);
            }
        });
        bt4.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                interpreter = new Interpreter(generator);
                try {
                    interpreter.interpret();
                } catch (InterpretException parseException) {
                    parseException.printStackTrace();
                }
                JFrame inf = new InfoFrame();
                inf.setTitle("解释执行");
                InfoFrame.text.setText(interpreter.getResults().toString());
                inf.setVisible(true);
            }
        });

        p.add(bt1);
        p.add(bt2);
        //p.add(bt3);
        p.add(bt3);
        p.add(bt4);
        return p;
    }

    public static String readFile(String fileName) throws IOException {
        StringBuilder sbr = new StringBuilder();
        String str;
        FileInputStream fis = new FileInputStream(fileName);
        BufferedInputStream bis = new BufferedInputStream(fis);
        InputStreamReader isr = new InputStreamReader(bis, StandardCharsets.UTF_8);
        BufferedReader in = new BufferedReader(isr);
        while ((str = in.readLine()) != null) {
            sbr.append(str).append('\n');
        }
        in.close();
        return sbr.toString();
    }

    public static void main(String[] args) {
        // TODO Auto-generated method stub
        MainFrame mf = new MainFrame();
        mf.setVisible(true);
    }
}

class FilePanel extends JPanel {
    FilePanel(String str) {
        JLabel label = new JLabel(str);
        JTextField fileText = new JTextField(35);
        JButton chooseButton = new JButton("浏览...");
        this.add(label);
        this.add(fileText);
        this.add(chooseButton);
        ClickAction ca = new ClickAction(this);
        chooseButton.addActionListener(ca);
    }

    public String getFileName() {
        JTextField jtf = (JTextField) this.getComponent(1);
        return jtf.getText();
    }

    // 按钮响应函数
    private class ClickAction implements ActionListener {
        private final Component cmpt;

        ClickAction(Component c) {
            cmpt = c;
        }

        @Override
        public void actionPerformed(ActionEvent event) {
            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File("."));
            int ret = chooser.showOpenDialog(cmpt);
            if (ret == JFileChooser.APPROVE_OPTION) {
                JPanel jp = (JPanel) cmpt;
                JTextField jtf = (JTextField) jp.getComponent(1);//获取zujian
                jtf.setText(chooser.getSelectedFile().getPath());
            }
        }
    }
}
