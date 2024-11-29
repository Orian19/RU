FROM python:alpine3.17

WORKDIR /app

COPY requirements.txt .
COPY stocks.py .

RUN pip install -r requirements.txt

ENV FLASK_APP=stocks.py
ENV FLASK_RUN_PORT=5001

EXPOSE 5001

CMD ["flask", "run", "--host=0.0.0.0"]
