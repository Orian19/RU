FROM python:alpine3.17

WORKDIR /stocks

COPY requirements.txt .
COPY stocks.py .

RUN pip install -r requirements.txt

ENV FLASK_APP=stocks.py

CMD ["flask", "run", "--host=0.0.0.0", "--port=8000"]
