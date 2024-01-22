def validate_israeli_id(id_number):
    """
    Multiply each digit by 1 or 2 in an alternating fashion. Left-most digit by 1, next digit by 2, then by 1,
    then by 2, and so on. For every multiplication result from the previous step, if it's greater than 9,
    sum the digits of that number to get a single digit number. Sum up all the numbers from the previous step. If the
    result is a multiply of 10, we can conclude that the ID number is valid. Otherwise, it's invalid. :param
    id_number: :return:
    """
    val_id = [int(d) for d in str(id_number)]
    val_id[1::2] = [d * 2 for d in val_id[1::2]]

    for i in range(1, len(val_id), 2):
        if val_id[i] > 9:
            val_id[i] = int(str(val_id[i])[0]) + int(str(val_id[i])[1])

    print("valid" if sum(val_id) % 10 == 0 else "invalid")


if __name__ == '__main__':
    validate_israeli_id(123456782)
