import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;

public class Test {
    public static void main(String[] args) {
        Instant start = Instant.parse("2023-12-18T23:23:28.762367Z");
        Instant end = Instant.parse("2024-01-08T23:23:13.087Z");
        System.out.println(getDaysBetweenUTC(end, start));
    }

    public static Integer getDaysBetween(Instant start, Instant end){

        System.out.println(" DAYS "+ end + " " + start );
        long l =  ChronoUnit.DAYS.between(end, start);
        int days = (int) l;
        System.out.println("### Between days " + days);
        return days;
    }

    public static Integer getDaysBetweenUTC(Instant start, Instant end){
        System.out.println(" DAYS " + start + " " + end );
        LocalDate startDateTime = LocalDate.ofInstant(
                start,
                ZoneId.of("UTC") // Assuming always 'Z'
        );
        LocalDate endDateTime = LocalDate.ofInstant(
                end,
                ZoneId.of("UTC") // Assuming always 'Z'
        );
        System.out.println(" DAYS " + endDateTime + " " + startDateTime );
        long l =  ChronoUnit.DAYS.between(endDateTime, startDateTime);
        int days = (int) l;
        System.out.println("### Between days " + days);
        return days;
    }


}
