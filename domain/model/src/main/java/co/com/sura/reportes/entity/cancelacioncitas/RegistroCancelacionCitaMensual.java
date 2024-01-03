package co.com.sura.reportes.entity.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
@ToString
@JsonPropertyOrder({"dia"})
public class RegistroCancelacionCitaMensual{
    private String dia;
    private Integer totalCitasCanceladas;
    private List<RegistroCancelacionCita> registros;

    public static List<RegistroCancelacionCitaMensual> ordenarByDia(List<RegistroCancelacionCitaMensual> registros){
        return registros.stream()
                .sorted(Comparator.comparing(registro->Integer.parseInt(registro.getDia())))
                .collect(Collectors.toList());
    }
}
