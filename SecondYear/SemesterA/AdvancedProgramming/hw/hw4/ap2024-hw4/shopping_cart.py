from item import Item
from errors import *


class ShoppingCart:
    def __init__(self):
        self._cart = []

    def get_cart(self) -> list:
        return self._cart

    def get_items_names(self) -> list:
        """
        gives a list of names of all the items in the cart
        :return: list of item names
        """
        items_names = []
        for item in self._cart:
            items_names.append(item.name)

        return items_names

    def get_items_hashtags(self) -> list:
        """
        gives a list of hashtags of all the items in the cart
        :return: list of item hashtags
        """
        items_hashtags = []
        for item in self._cart:
            for tag in item.hashtags:
                items_hashtags.append(tag)

        return items_hashtags

    def add_item(self, item: Item):
        """
        adds a given item to the shopping cart
        :param item: instance of item
        :return:
        """
        if item not in self._cart:
            self._cart.append(item)
        else:
            raise ItemAlreadyExistsError()

    def remove_item(self, item_name: str):
        """
        removes the item with the given name from the shopping cart
        :param item_name: instance of str
        :return:
        """
        exists = False
        for item in self._cart:
            if item.name == item_name:  # checking if an item with a given is in the cart
                self._cart.remove(item)
                exists = True

        if not exists:
            raise ItemNotExistError

    def get_subtotal(self) -> int:
        """
        returns the subtotal price of all the items currently in the shopping cart
        :return: subtotal of the current cart instance
        """
        subtotal = 0
        for item in self._cart:
            subtotal += item.price

        return subtotal
