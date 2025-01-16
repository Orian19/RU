from flask import Flask, request, jsonify
import requests
import os

app = Flask(__name__)

@app.route('/capital-gains', methods=['GET'])
def stocks():
    """
    GET request: total capital gain of the portfolio
    """
    if request.method == 'GET':
        try:
            stocks = requests.get('http://stocks-service/stocks').json()

            if 'numsharesgt' in request.args:
                num_shares_gt = int(request.args.get('numsharesgt'))
                stocks = [stock for stock in stocks if stock['shares'] > num_shares_gt]
            if 'numshareslt' in request.args:
                num_shares_lt = int(request.args.get('numshareslt'))
                stocks = [stock for stock in stocks if stock['shares'] < num_shares_lt]
            
            updated_portfolio_value = 0
            response = requests.get('http://stocks-service/portfolio-value')
            if response.status_code == 200:
                updated_portfolio_value = response.json().get('portfolio value', 0)
            total_cost = sum(stock['purchase price'] * stock['shares'] for stock in stocks)
            capital_gain = round(float(updated_portfolio_value - total_cost), 2)

            return jsonify({"capital gain": capital_gain}), 200
        except Exception as e:
            print("Exception: ", str(e))
            return jsonify({'server error': str(e)}), 500

@app.route('/kill', methods=['GET'])
def kill_container():
    """
    GET request: Shutdown the server
    """
    if request.method == 'GET':
        os._exit(0)
