package gui;
/**
 * @Author: zlxx
 * @Date: 2020/12/4 21:19
 */

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.*;
import java.nio.charset.StandardCharsets;

public class InfoFrame extends JFrame {

    private static final long serialVersionUID = 8766059377195109228L;
    private static String title;
    private static String fileName;

    public static TextArea text;

    public InfoFrame() {
        init();

    }
    public InfoFrame(String title,String fileName){
        InfoFrame.title =title;
        InfoFrame.fileName =fileName;
        init();
        this.setTitle(title);
        try {
            readFile(fileName);
        } catch (IOException e) {

            e.printStackTrace();
        }
    }
    private void init() {
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        Dimension screen = toolkit.getScreenSize();
        setSize(700, 600);
        super.setLocation(screen.width / 2 - this.getWidth() / 2, screen.height
                / 2 - this.getHeight() / 2);
        setContentPane(createContentPane());
    }

    private Container createContentPane() {
        JPanel pane = new JPanel(new BorderLayout());
        text = new TextArea();
        text.setForeground(Color.black);
        pane.add(BorderLayout.CENTER, text);
        return pane;
    }
    private String readFile(String filename)
            throws IOException{
        StringBuilder sbr = new StringBuilder();
        String str;
        FileInputStream fis = new FileInputStream(filename);
        BufferedInputStream bis = new BufferedInputStream(fis);
        InputStreamReader isr = new InputStreamReader(bis, StandardCharsets.UTF_8);
        BufferedReader in=new BufferedReader(isr);
        while((str=in.readLine())!=null){
            sbr.append(str).append('\n');
        }
        in.close();
        text.setText(sbr.toString());
        return sbr.toString();
    }
    public static String getTitl() {
        return title;
    }

    public static void setTitl(String title) {
        InfoFrame.title = title;
    }

    public static String getFileName() {
        return fileName;
    }

    public static void setFileName(String fileName) {
        InfoFrame.fileName = fileName;
    }

    public static TextArea getText() {
        return text;
    }

    public static void setText(TextArea jText) {
        InfoFrame.text = jText;
    }


}
