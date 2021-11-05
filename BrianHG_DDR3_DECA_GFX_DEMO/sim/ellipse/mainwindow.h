#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QPainter>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

protected:
    void paintEvent(QPaintEvent * pe) override;
    void setPixel(int x, int y) { QPainter p(this); QPoint(x, y); p.drawPoint(x, y); }
    void drawLine(int x0, int y0, int x1, int y1, QColor c) { QPainter p(this); p.setPen(QPen(c)); QPoint a = QPoint(x0, y0); QPoint b = QPoint(x1, y1); p.drawLine(a, b); }
    void startWorkInAThread(int argc, char *argv[]);
    void handleResults(int x, int y, int col);
    QPixmap pixmap;
private:
    Ui::MainWindow *ui;
};
#endif // MAINWINDOW_H
