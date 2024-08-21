import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ConcatenateColumn {
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
    }
}
