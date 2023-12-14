package co.com.sura.reportes.entity.cancelacioncitas;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ResumenCancelacionCita {
    private int totalCancelaciones;
    private List<RegistroCancelacionCita> registrosCancelacion;
}
