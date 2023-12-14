def split(some_list: list[int]) -> dict[int, int]:
    res = {}

    for el in some_list:
        res[el % 2] = res.get(el % 2, 0) + 1

    return res
