package co.com.sura.entity.reportes.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class RegistroCancelacionCitaMensual extends RegistroCancelacionCita{
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String dia;
}
