import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class PostgresqlJdbcConnector {
    public static void main(String[] args) {
        int columnNumber = 1;  // Specify the column number to concatenate (e.g., 1 for the first column)
        String delimiter = ",";  // Specify the delimiter used in the CSV file (e.g., comma)

        // Check if the CSV file is provided as an argument
//        if (args.length == 0) {
//            System.out.println("Usage: java ConcatenateColumn <input_file.csv>");
//            System.exit(1);
//        }

        String inputFile = "/Users/chunguang.wang/MyDocuments/Hulu/invoice/extract-2024-01-04T23_16_23.210Z_sprint_not_switch_users.csv";

        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            StringBuilder concatenatedColumn = new StringBuilder();

            String line;
            while ((line = reader.readLine()) != null) {
                String[] columns = line.split(delimiter);

                // Check if the column number is valid
                if (columnNumber > 0 && columnNumber <= columns.length) {
                    String columnValue = "(" + columns[columnNumber - 1].trim() + ")";
                    concatenatedColumn.append(columnValue).append(",");
                }
            }

            System.out.println(concatenatedColumn.toString().trim());
        } catch (IOException e) {
            e.printStackTrace();
        }
        // Database connection parameters
        String url = "jdbc:postgresql://jive-async-request-master.prod.hulu.com:5432/jive_async_request_v2";
        String user = "jive_async_request_app";
        String password = "iLJaVbJXBm5v";

        // Establish a connection
        try (Connection connection = DriverManager.getConnection(url, user, password)) {
            System.out.println("Connected to the PostgreSQL server successfully.");
            // Perform database operations here
        } catch (SQLException e) {
            System.out.println("Connection failure.");
            e.printStackTrace();
        }
    }
}
