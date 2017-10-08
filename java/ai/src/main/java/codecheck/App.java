package codecheck;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class App {

    private static int RECURSION_LIMIT = 100000;

    private enum ExpectedOutcome {
        WIN, LOSE, UNCERTAIN
    }

    private static String[] wordStrings;

    private static ArrayList<Integer> filterWords(String lastWord, ArrayList<Integer> words) {
        ArrayList<Integer> newWords = new ArrayList<>();
        char lastChar = lastWord.charAt(lastWord.length()-1);
        for (Integer word : words) {
            if (wordStrings[word].charAt(0) == lastChar) {
                newWords.add(word);
            }
        }
        return newWords;
    }

    private static ExpectedOutcome tryCandidate(Integer candidate, ArrayList<Integer> words, int depth, int maxDepth) {
        if (depth >= maxDepth) {
            return ExpectedOutcome.UNCERTAIN;
        }
        ArrayList<Integer> wordsWithoutCandidate = (ArrayList<Integer>) words.clone();
        wordsWithoutCandidate.remove(candidate);
        ArrayList<Integer> nextWords = filterWords(wordStrings[candidate], wordsWithoutCandidate);
        for (Integer nextWord : nextWords) {
            switch (tryCandidate(nextWord,
                    wordsWithoutCandidate,
                    depth+1,
                    Math.min(maxDepth, RECURSION_LIMIT / nextWords.size()))) {
                case WIN:
                    // The enemy will have a winning move
                    return ExpectedOutcome.LOSE;
                case UNCERTAIN:
                    // There's a deep and wide recursion going on
                    return ExpectedOutcome.UNCERTAIN;
                default:
                    break;
            }
        }
        // The enemy will have no winning moves
        return ExpectedOutcome.WIN;
    }

    private static String solution (String lastWord, ArrayList<Integer> words) {
        ArrayList<Integer> candidates = filterWords(lastWord, words);
        ArrayList<Integer> uncertainCandidates = new ArrayList<>();
        if (candidates.size() == 0) {
            return "";
        }

        // Look for a sure win
        for (Integer candidate : candidates) {
            switch (tryCandidate(candidate, words, 0, RECURSION_LIMIT)) {
                case WIN:
                    return wordStrings[candidate];
                case UNCERTAIN:
                    uncertainCandidates.add(candidate);
                    break;
                default:
                    break;
            }
        }
        // No candidate results in a sure win. Pick a random one preferring uncertain options
        // But will lose anyway against an intelligent ai
        if (uncertainCandidates.size() > 0) {
            return wordStrings[uncertainCandidates.get((new Random()).nextInt(uncertainCandidates.size()))];
        }
        return wordStrings[candidates.get((new Random()).nextInt(candidates.size()))];
    }

	public static void main(String[] args) {
        if (args.length < 1) {
            System.exit(1);
        }
        wordStrings = Arrays.copyOfRange(args, 1, args.length);
        ArrayList<Integer> indexes = new ArrayList<>();
        for (int i = 0; i < args.length - 1; i++) {
            indexes.add(i);
        }
        System.out.print(solution(args[0], indexes));
	}
}
