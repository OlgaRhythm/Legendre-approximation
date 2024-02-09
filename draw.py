import matplotlib.pyplot as plt

# Функция для чтения координат из файла
def read_coordinates(file_name):
    with open(file_name, 'r') as file:
        lines = file.readlines()
        x_coords = []
        y_coords = []
        for line in lines:
            x, y = map(float, line.strip().split())
            x_coords.append(x)
            y_coords.append(y)
    return x_coords, y_coords

# Чтение координат из первого файла
file1_x_coords, file1_y_coords = read_coordinates('input.txt')

# Чтение координат из второго файла
file2_x_coords, file2_y_coords = read_coordinates('output.txt')

# Создание графика и отображение точек из первого файла
plt.scatter(file1_x_coords, file1_y_coords, label='input')

# Отображение точек из второго файла и их соединение линиями
plt.plot(file2_x_coords, file2_y_coords, marker='o', linestyle='-', color='orange', label='output')

# Настройка легенды, осей и заголовка
plt.legend()
plt.xlabel('X-координаты')
plt.ylabel('Y-координаты')
plt.title('График точек из файлов')

# Показать график
plt.show()