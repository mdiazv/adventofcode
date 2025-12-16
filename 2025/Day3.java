import java.util.List;
import java.util.Scanner;

record Bank(String s) {
    long maxJoltage() {
        long jolt = 0;
        for (int i = 0; i < s.length()-1; i++) {
            for (int j = i+1; j < s.length(); j++) {
                jolt = Math.max(jolt, Long.parseLong("" + s.charAt(i) + s.charAt(j)));
            }
        }
        return jolt;
    }
    private int nextBattery(int start, int need) {
        return "987654321".chars()
                .map(s.substring(start)::indexOf)
                .filter(j -> j >= 0 && start+j <= s.length()-need)
                .findFirst()
                .getAsInt();
    }
    long ultraMaxJoltage() {
        int k = 0, N = s.length();
        StringBuilder sb = new StringBuilder(12);
        for (int need = 12; need > 0; need--) {
            int p = nextBattery(k, need);
            sb.append(s.charAt(k+p));
            k += p+1;
        }
        return Long.parseLong(sb.toString());
    }
}

void main () {
    List<Bank> banks = new Scanner(System.in)
            .useDelimiter("\n")
            .tokens()
            .map(Bank::new)
            .toList();
    long totalJoltage = banks.stream()
            .mapToLong(Bank::maxJoltage)
            .sum();
    IO.println(String.format("Total joltage: %d", totalJoltage));
    long ultraJoltage = banks.stream()
            .mapToLong(Bank::ultraMaxJoltage)
            .sum();
    IO.println(String.format("Total joltage: %d", ultraJoltage));
}