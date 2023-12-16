package co.com.sura.reportes.entity.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

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

}
