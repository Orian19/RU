import yaml

from errors import *

from item import Item
from shopping_cart import ShoppingCart


class Store:
    def __init__(self, path):
        with open(path) as inventory:
            items_raw = yaml.load(inventory, Loader=yaml.FullLoader)['items']
        self._items = self._convert_to_item_objects(items_raw)
        self._shopping_cart = ShoppingCart()

    @staticmethod
    def _convert_to_item_objects(items_raw):
        return [Item(item['name'],
                     int(item['price']),
                     item['hashtags'],
                     item['description'])
                for item in items_raw]

    def get_items(self) -> list:
        return self._items

    def tags_sort(self, item):
        """
        gives the correct sorting order for the sort function. sorting based on tags commonality and then by name
        :param item:
        :return:
        """
        tags = self._shopping_cart.get_items_hashtags()

        common_hashtags = 0
        for hashtag in item.hashtags:
            common_hashtags += tags.count(hashtag)

        return -common_hashtags, item.name  # will sort by: tags(minus of decreasing order), name

    def search_by_name(self, item_name: str) -> list:
        """
        searches for a specific item in the store by its name (not exact match)
        :param item_name: instance of str
        :return: a sorted list of all the items that match the search term
        """
        found_list = []
        for item_store in self._items:  # adding to the list only items that are not in the cart
            if item_name in item_store.name and item_store.name not in self._shopping_cart.get_items_names():
                found_list.append(item_store)

        found_list.sort(key=self.tags_sort)  # sorting the list

        return found_list

    def search_by_hashtag(self, hashtag: str) -> list:
        """
        searches for a specific item in the store by its hashtag (exact match)
        :param hashtag: instance of str
        :return: a sorted list of all the items matching the search criterion
        """
        found_list = []
        for item_store in self._items:  # adding to the list only items that are not in the cart
            if hashtag in item_store.hashtags and item_store.name not in self._shopping_cart.get_items_names():
                found_list.append(item_store)

        found_list.sort(key=self.tags_sort)

        return found_list

    def add_item(self, item_name: str):
        """
        adds an item with the given name to the customer’s shopping cart
        :param item_name: instance of str
        :return:
        """
        in_cart = False
        mult_match = 0
        item_to_add = None

        for item_store in self._items:
            if item_name in item_store.name:
                item_to_add = item_store
                mult_match += 1

        cart_items_names = self._shopping_cart.get_items_names()
        for item_cart in cart_items_names:
            if item_name in item_cart:
                in_cart = True

        if in_cart:
            raise ItemAlreadyExistsError
        elif mult_match > 1:
            raise TooManyMatchesError
        elif mult_match == 0:
            raise ItemNotExistError
        else:
            self._shopping_cart.add_item(item_to_add)

    def remove_item(self, item_name: str):
        """
        removes an item with the given name from the customer’s shopping cart
        :param item_name: instance of str
        :return:
        """
        mult_match = 0

        for item_store in self._items:
            if item_name in item_store.name:
                mult_match += 1

        if mult_match > 1:
            raise TooManyMatchesError
        elif mult_match == 0:
            raise ItemNotExistError
        else:
            self._shopping_cart.remove_item(item_name)

    def checkout(self) -> int:
        """
        returns the total price of all the items in the costumer’s shopping cart
        :return: total price in current cart
        """
        return self._shopping_cart.get_subtotal()
