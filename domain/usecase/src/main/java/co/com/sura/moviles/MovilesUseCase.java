package co.com.sura.moviles;

import co.com.sura.reportes.gateway.ReportesRepository;

public class MovilesUseCase {

    private final ReportesRepository movilRepository;

    public MovilesUseCase(ReportesRepository movilRepository) {
        this.movilRepository = movilRepository;
    }
}
