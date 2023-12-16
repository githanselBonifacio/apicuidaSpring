package co.com.sura.reportes.entity.cancelacioncitas;


import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.*;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
@ToString
@JsonPropertyOrder({"mes"})
public class RegistroCancelacionCitaAnual {
    private String mes;
    private Integer totalCitasCanceladas;
    private List<RegistroCancelacionCita> registros;


    public static RegistroCancelacionCitaAnual converNumeroToNombreMes(RegistroCancelacionCitaAnual registro){
        int numeroMes = Integer.parseInt(registro.getMes());
        String nombreMes = Month.of(numeroMes)
                .getDisplayName(TextStyle.FULL, new Locale("es", "ES"));
        registro.setMes(nombreMes);
        return registro;
    }
    public static List<RegistroCancelacionCitaAnual> ordenarListaByMes(
            Collection<RegistroCancelacionCitaAnual> registros){
        return registros.stream()
                .sorted(Comparator.comparingInt(registro -> Integer.parseInt(registro.getMes())))
                .map(RegistroCancelacionCitaAnual::converNumeroToNombreMes)
                .collect(Collectors.toList());


    }
}
