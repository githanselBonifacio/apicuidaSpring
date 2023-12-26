package co.com.sura.genericos;

import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Response <T>{
    private Integer status;
    private Boolean flag;
    private String message;
    private String tecnicalMessage;
    private String detail;
    private T result;

}
