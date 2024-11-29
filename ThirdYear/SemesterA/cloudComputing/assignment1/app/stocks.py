from flask import Flask, request, jsonify
from datetime import datetime
import requests
from uuid import uuid4
import re

app = Flask(__name__)
app.config['API_KEY'] = "" # put ninja api here
Stocks = {}

@app.route('/stocks', methods=['POST', 'GET'])
def stocks():
    """
    POST request: Add a new stock to the portfolio
    GET request: Retrieve all stocks in the portfolio
    """
    if request.method == 'POST':
        try:
            content_type = request.headers.get('Content-Type')
            if content_type != 'application/json':
                return jsonify({'error': 'Expected application/json media type'}), 415
            
            data = request.get_json()
            required_fields = ['symbol', 'purchase price', 'shares']
            optional_fields = ['name', 'purchase date']
            allowed_fields = set(required_fields + optional_fields)

            if not all(field in data for field in required_fields):
                return jsonify({'error': 'Malformed data'}), 400
            
            if not set(data.keys()).issubset(allowed_fields):
                return jsonify({'error': 'Malformed data'}), 400

            new_id = uuid4()

            if 'purchase date' not in data:
                data['purchase date'] = 'N/A'
            else:
                date_pattern = r'^\d{2}-\d{2}-\d{4}$'
                if not re.match(date_pattern, data['purchase date']):
                    return jsonify({'error': 'Malformed data'}), 400
                data['purchase date'] = data['purchase date']
            if 'name' not in data:
                data['name'] = 'N/A'
            else:
                data['name'] = data['name']

            stock = {
                'id': new_id,
                'name': data['name'],
                'symbol': data['symbol'],
                'purchase date': data['purchase date'],
                'purchase price': data['purchase price'],
                'shares': data['shares']
            }

            for stock_id in Stocks:
                if Stocks[stock_id]['symbol'] == stock['symbol']:
                    return jsonify({'error': 'Malformed data'}), 400

            Stocks[str(new_id)] = stock
            respsnse_data = {"id": new_id}
            return jsonify(respsnse_data), 201
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
    else:  # GET request
        try:
            stocks = list(Stocks.values())
            print(stocks)
            if 'purchase date' in request.args:
                purchase_date = request.args.get('purchase date')
                stocks = [stock for stock in stocks if stock['purchase date'] == purchase_date]
            if 'symbol' in request.args:
                symbol = request.args.get('symbol')
                stocks = [stock for stock in stocks if stock['symbol'] == symbol]
            if 'name' in request.args:
                name = request.args.get('name')
                stocks = [stock for stock in stocks if stock['name'] == name]
            if 'purchase price' in request.args:
                purchase_price = request.args.get('purchase price')
                stocks = [stock for stock in stocks if stock['purchase price'] == round(float(purchase_price), 2)]
            if 'shares' in request.args:
                shares = request.args.get('shares')
                stocks = [stock for stock in stocks if stock['shares'] == int(shares)]
                print(stocks)
            return jsonify(stocks), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
        
@app.route('/stocks/<id>', methods=['GET', 'PUT', 'DELETE'])
def stock(id):
    """
    GET request: Retrieve a stock by ID
    PUT request: Update a stock by ID
    DELETE request: Delete a stock by ID
    """
    if request.method == 'GET':
        try:
            stock = Stocks.get(id)
            if stock is None:
                return jsonify({'error': 'Not found'}), 404
            return jsonify(stock), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
    elif request.method == 'PUT':
        try:
            stock = Stocks.get(id)
            if stock is None:
                return jsonify({'error': 'Not found'}), 404
            data = request.get_json()
            content_type = request.headers.get('Content-Type')
            if content_type != 'application/json':
                return jsonify({'error': 'Expected application/json media type'}), 415
            
            required_fields = ['id', 'symbol', 'purchase price', 'shares', 'purchase date', 'name']
            if not all(field in data for field in required_fields):
                return jsonify({'error': 'Malformed data'}), 400
            if data['id'] != id:
                return jsonify({'error': 'Malformed data'}), 400
            if 'purchase date' in data:
                stock['purchase date'] = data['purchase date']
            if 'name' in data:
                stock['name'] = data['name']
            if 'purchase price' in data:
                stock['purchase price'] = data['purchase price']
            if 'shares' in data:
                stock['shares'] = data['shares']

            Stocks[id] = stock
            return jsonify({"id": id}), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
    else:  # DELETE request
        try:
            del Stocks[id]
            return '', 204
        except KeyError:
            print("GET reuest error: No such ID")
            return jsonify({'error': 'Not found'}), 404
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500

          
@app.route('/stock-value/<id>', methods=['GET'])
def stock_value(id):
    """
    GET request: Retrieve the value of a stock by ID
    """
    if request.method == 'GET':
        try:
            stock = Stocks.get(id)

            if stock is None:
                return jsonify({'error': 'Not found'}), 404

            symbol = stock['symbol']
            shares = stock['shares']

            api_url = f'https://api.api-ninjas.com/v1/stockprice?ticker={symbol}'
            response = requests.get(api_url, headers={'X-Api-Key': app.config['API_KEY']})
            
            if response.status_code == requests.codes.ok:     
                stock_price = response.json()['price']
                stock_value = stock_price * shares
                return jsonify({
                    'symbol': symbol,
                    'ticker': stock_price,
                    'stock value': stock_value
                    }), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
    
@app.route('/portfolio-value', methods=['GET'])
def portfolio():
    """
    GET request: Retrieve the total value of the portfolio
    """
    if request.method == 'GET':
        try:
            total_value = 0
            for stock in Stocks.values():
                symbol = stock['symbol']
                shares = stock['shares']
                api_url = f'https://api.api-ninjas.com/v1/stockprice?ticker={symbol}'
                response = requests.get(api_url, headers={'X-Api-Key': app.config['API_KEY']})
                if response.status_code == requests.codes.ok:
                    stock_price = response.json()['price']
                    stock_value = stock_price * shares
                    total_value += stock_value
            return jsonify({ 
                'date':  datetime.now().strftime('%d-%m-%Y'),  # format DD-MM-YYYY
                'portfolio value': total_value
                }), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
