def remove_forbidden_characters(sentence, forbidden_characters):
    sen_to_print = ''.join(ch for ch in sentence if ch not in forbidden_characters)
    print(sen_to_print)


if __name__ == '__main__':
    remove_forbidden_characters("Awesome sentence!", "aeos")
