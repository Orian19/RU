def reverse(word):
    return word[::-1]


def is_palindrome(word):
    return word == reverse(word)


def find_largest_palindrome():
    largest_pal = 0
    for num1 in range(1000, 100, -1):
        for num2 in range(1000, 100, -1):
            cur_largest = num1 * num2
            if is_palindrome(str(cur_largest)) and cur_largest > largest_pal:
                largest_pal = cur_largest

    print(f"{largest_pal},{str(largest_pal)[::2]}")


if __name__ == '__main__':
    print(reverse("hello"))
    print(is_palindrome("radar"))
    find_largest_palindrome()
