package codecheck;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;

public class App {

    private static ArrayList<String> words;
    private static ArrayList<Ai> ais;

    private static void removeBogusWords (String lastWord) {
        char[] lastChars = new char[words.size()+1];
        for (int i = 0; i < words.size(); i++) {
            String word = words.get(i);
            lastChars[i] = word.charAt(word.length()-1);
        }
        lastChars[lastChars.length-1] = lastWord.charAt(lastWord.length()-1);

        ArrayList<String> newWords = new ArrayList<>();
        wordLoop: for (String word : words) {
            for (char lastChar : lastChars) {
                if (word.charAt(0) == lastChar) {
                    newWords.add(word);
                    continue wordLoop;
                }
            }
        }
        words = newWords;
    }

    private static void rotateAis() {
        java.util.Collections.rotate(ais, 1);
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 3) {
            System.exit(1);
        }
        ais = new ArrayList<>();
        ais.add(new Ai("FIRST", args[0]));
        ais.add(new Ai("SECOND", args[1]));
        String lastWord = args[2];
        String[] wordsFromArgs = Arrays.copyOfRange(args, 3, args.length);
        words = new ArrayList<>(Arrays.asList(wordsFromArgs));
        removeBogusWords(lastWord);

        while (true) {
            Ai ai = ais.get(0);
            String answer = ai.run(lastWord, words);
            boolean correct = ai.processAnswer(answer, lastWord, words);
            if (!correct) {
                System.out.println("WIN - " + (ais.get(1).getName()));
                System.exit(0);
            }
            words.remove(answer);
            lastWord = answer;
            rotateAis();
        }
    }
}

class Ai {
    private String name;
    private String script;

    Ai (String name, String script) {
        this.name = name;
        this.script = script;
    }

    String getName() {
        return name;
    }

    String run (String lastWord, ArrayList<String> words) throws IOException {
        String[] args = new String[words.size() + 2];
        args[0] = script;
        args[1] = lastWord;
        for (int i = 0; i < words.size(); i++) {
            args[i+2] = words.get(i);
        }
        Process process = new ProcessBuilder(args).start();
        BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));

        String line = reader.readLine();
        if (line != null) {
            return line;
        }
        return "";
    }

    boolean processAnswer(String answer, String lastWord, ArrayList<String> words) {
        boolean correct = true;
        if (answer.equals("")) {
            correct = false;
        } else {
            for (String word : words) {
                if (word.equals(answer)) {
                    correct = true;
                }
            }
            if (lastWord.charAt(lastWord.length() - 1) != answer.charAt(0)) {
                correct = false;
            }
        }
        String result = correct ? "OK" : "NG";
        System.out.println(String.format("%s (%s): %s", name, result, answer));
        return correct;
    }
}
