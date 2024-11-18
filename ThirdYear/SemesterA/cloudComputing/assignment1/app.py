from flask import Flask, request, jsonify
from uuid import uuid4

app = Flask(__name__)
app.config['API_KEY'] = "ypEWmvAVIB0S4viTgRJQdw==a119mdlgYyq9UhLW"
Stocks = {}

@app.route('/stocks', methods=['POST', 'GET'])
def stocks():
    if request.method == 'POST':
        try:
            content_type = request.headers.get('Content-Type')
            if content_type != 'application/json':
                return jsonify({'error': 'Expected application/json media type'}), 415
            
            data = request.get_json()
            required_fields = ['symbol', 'purchase price', 'shares']

            if not all(field in data for field in required_fields):
                return jsonify({'error': 'Malformed data'}), 400
            
            new_id = uuid4()

            if 'purchase date' not in data:
                data['purchase date'] = 'N/A'
            else:
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

            Stocks[str(new_id)] = stock
            respsnse_data = {"id": new_id}
            return jsonify(respsnse_data), 201
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500
    else:  # GET request
        try:
            stocks = Stocks.values()
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
            if 'purchase date' in data:
                stock['purchase date'] = data['purchase date']
            if 'name' in data:
                stock['name'] = data['name']
            if 'purchase price' in data:
                stock['purchase price'] = data['purchase price']
            if 'shares' in data:
                stock['shares'] = data['shares']

            Stocks[id] = stock
            return jsonify(Stocks[id]), 200
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

if __name__ == '__main__':
    app.run(port=8000, debug=True)
