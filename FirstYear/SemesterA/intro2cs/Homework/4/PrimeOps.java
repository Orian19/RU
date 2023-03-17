// Represents algebraic operations as functions.
public class PrimeOps {
	public static void main(String args[]) {
	}  

	// Returns an array of all prime numbers up to any given number.
	// Assumption: n is nonnegative.
	public static int[] sieve(int n) {
		if (n == 1 || n == 0) {
			int [] arr = {};
			return arr;
		} else if (n == 2) {
			int [] arr = {2};
			return arr;
		}

		n += 1;

		boolean[] isPrimes = new boolean[n];
		for (int i = 0; i < n; i++) {
			isPrimes[i] = true;
		}

		for (int i = 2; i < n; i++) {
			if (isPrimes[i]) {
				for (int j = 2; j < n; j++) {
					if (j % i == 0 && i != j) {
						isPrimes[j] = false;
					}
				}
			}
		}

		int countT = 0;
		for (int i = 2; i < n; i++) {
			if (isPrimes[i]) {
				countT++;
			}
		}

		int [] primes = new int [countT];
		int primeIndex = 0;
		for (int i = 2; i < n; i++) {
			if (isPrimes[i] && i > 1) {
				primes[primeIndex] = i;
				primeIndex++;
			}
		}

		return primes;
	}
}
