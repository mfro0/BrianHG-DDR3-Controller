#include "mainwindow.h"
#include "./ui_mainwindow.h"
#include <QPainter>
#include <iostream>
#include <QBitmap>
#include <QApplication>
#include <QThread>
#include <QPixmap>
#include <QPaintEvent>
#include <QColor>



extern "C" int ghdl_main(int argc, char *argv[]);

class WorkerThread : public QThread
{
    Q_OBJECT
    int argc;
    char **argv;

    void run() override {
        QString result;
        std::cout << "starting worker" << std::endl;
        ghdl_main(argc, argv);
    }

public:
    WorkerThread(int argc, char *argv[]) : argc{argc}, argv{argv} {};
signals:
    void resultReady(int x, int y, int col);
};

WorkerThread *workerThread;

void cppPlot(int x, int y, int col)
{
    emit workerThread->resultReady(x, y, col);
}

extern "C" void plot(int x, int y, int col)
{
    cppPlot(x, y, col);
}

void MainWindow::handleResults(int x, int y, int col)
{
    QPainter p(&pixmap);
    QColor c(col);
    p.setPen(QColor(col >> 16, col >> 8 & 0xff, col & 0xff));
    p.drawPoint(QPoint(x, y));
    repaint(QRect(x, y, 1, 1));
}

void MainWindow::startWorkInAThread(int argc, char *argv[])
{
    workerThread = new WorkerThread(argc, argv);
    connect(workerThread, &WorkerThread::resultReady, this, &MainWindow::handleResults);
    connect(workerThread, &WorkerThread::finished, workerThread, &QObject::deleteLater);

    workerThread->start();
}

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    int argc = 1;
    char *argv[] = {"argument1", "argument2"};
    ui->setupUi(this);
    pixmap = QPixmap(this->width(), this->height());
    pixmap.fill(QColor(QColorConstants::White));
    std::cout << "start thread" << std::endl;
    startWorkInAThread(argc, argv);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::paintEvent(QPaintEvent * pe)
{
    QRect r = pe->rect();
    QPainter p(this);
    p.drawPixmap(r, pixmap, r);
}

#include "mainwindow.moc"
