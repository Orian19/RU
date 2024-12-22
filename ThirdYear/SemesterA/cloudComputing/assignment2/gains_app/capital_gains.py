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
            stocks = []
            stocks1 = [] 
            stocks2 = []
          
            if 'portfolio' in request.args:
                portfolio = request.args.get('portfolio')
                if portfolio == 'stocks1':
                 
                    stocks = requests.get('http://stock1-1-robust:8000/stocks').json()
                    stocks1 = stocks
                    
                elif portfolio == 'stocks2':
                  
                    stocks = requests.get('http://stock2-robust:8000/stocks').json()
                    stocks2 = stocks
                 
            else:
                # Fetch stocks for both stock1 and stock2
                stocks1 = requests.get('http://stock1-1-robust:8000/stocks').json()
                stocks2 = requests.get('http://stock2-robust:8000/stocks').json()
                stocks = stocks1 + stocks2  # Combine both portfolios

            if 'numsharesgt' in request.args:
                num_shares_gt = int(request.args.get('numsharesgt'))
                stocks = [stock for stock in stocks if stock['shares'] > num_shares_gt]
            if 'numshareslt' in request.args:
                num_shares_lt = int(request.args.get('numshareslt'))
                stocks = [stock for stock in stocks if stock['shares'] < num_shares_lt]

            updated_portfolio_value = 0
            for stock in stocks:
                stock_id = stock['_id']
                if stock in stocks1:
                    stock_value_response = requests.get(f'http://stock1-1-robust:8000/stock-value/{str(stock_id)}')
                elif stock in stocks2:
                    stock_value_response = requests.get(f'http://stock2-robust:8000/stock-value/{str(stock_id)}')
                
                if stock_value_response.status_code == 200:
                    stock_value = stock_value_response.json()['stock value']
                    updated_portfolio_value += stock_value
                
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
