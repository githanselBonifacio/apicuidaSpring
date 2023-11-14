package co.com.sura.entity.reportes.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.Locale;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
@JsonPropertyOrder({"mes"})
public class RegistroCancelacionCitaAnual extends RegistroCancelacionCita {
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String mes;

    public static RegistroCancelacionCitaAnual converNumeroToNombreMes(RegistroCancelacionCitaAnual registro){
        int numeroMes = Integer.parseInt(registro.getMes());
        String nombreMes = Month.of(numeroMes)
                .getDisplayName(TextStyle.FULL, new Locale("es", "ES"));
        registro.setMes(nombreMes);
        return registro;
    }

    @Override
    public String toString() {
        return "RegistroCancelacionCitaAnual{" +
                "mes='" + mes + '\'' +
                "descripcion='" + getDescripcion() + '\'' +
                "cantida='" + getCantidad() + '\'' +
                '}';
    }
}
