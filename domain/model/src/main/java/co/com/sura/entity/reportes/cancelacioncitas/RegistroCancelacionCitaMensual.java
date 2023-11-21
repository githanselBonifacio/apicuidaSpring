package co.com.sura.entity.reportes.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
@JsonPropertyOrder({"dia"})
public class RegistroCancelacionCitaMensual{
    private String dia;
    private Integer totalCitasCanceladas;
    private List<RegistroCancelacionCita> registros;

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
